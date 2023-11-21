//
// This file is subject to the terms and conditions defined in
// file 'LICENSE', which is part of this source code package.
//

#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(feature = "std")]
pub use std::*;

use codec::{Decode, Encode};
use frame_support::{
	dispatch::{DispatchClass, DispatchInfo, PostDispatchInfo},
	traits::Get,
};
pub use pallet::*;
use pallet_transaction_payment::OnChargeTransaction;
use scale_info::TypeInfo;
#[cfg(feature = "std")]
pub use serde::*;
use sp_runtime::{
	traits::{
		DispatchInfoOf, Dispatchable, One, PostDispatchInfoOf, SaturatedConversion, Saturating,
		SignedExtension,
	},
	transaction_validity::{
		InvalidTransaction, TransactionLongevity, TransactionPriority, TransactionValidity,
		TransactionValidityError, ValidTransaction,
	},
	DispatchResult, FixedPointOperand,
};
use sp_std::prelude::*;
use up_sponsorship::SponsorshipHandler;

#[frame_support::pallet]
mod pallet {
	use super::*;

	#[pallet::config]
	pub trait Config: frame_system::Config + pallet_transaction_payment::Config {
		type SponsorshipHandler: SponsorshipHandler<Self::AccountId, Self::RuntimeCall>;
	}

	#[pallet::pallet]
	pub struct Pallet<T>(_);
}

type BalanceOf<T> = <<T as pallet_transaction_payment::Config>::OnChargeTransaction as pallet_transaction_payment::OnChargeTransaction<T>>::Balance;

/// Require the transactor pay for themselves and maybe include a tip to gain additional priority
/// in the queue.
#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
pub struct ChargeTransactionPayment<T: Config>(#[codec(compact)] BalanceOf<T>);

impl<T: Config + Send + Sync> ChargeTransactionPayment<T> {
	/// Create new `SignedExtension`
	pub fn new(tip: BalanceOf<T>) -> Self {
		Self(tip)
	}
}

impl<T: Config + Send + Sync> sp_std::fmt::Debug for ChargeTransactionPayment<T> {
	#[cfg(feature = "std")]
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "ChargeTransactionPayment<{:?}>", self.0)
	}
	#[cfg(not(feature = "std"))]
	fn fmt(&self, _: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		Ok(())
	}
}

impl<T: Config> ChargeTransactionPayment<T>
where
	T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
	BalanceOf<T>: Send + Sync + From<u64> + FixedPointOperand,
{
	pub fn traditional_fee(
		len: usize,
		info: &DispatchInfoOf<T::RuntimeCall>,
		tip: BalanceOf<T>,
	) -> BalanceOf<T>
	where
		T::RuntimeCall: Dispatchable<Info = DispatchInfo>,
	{
		<pallet_transaction_payment::Pallet<T>>::compute_fee(len as u32, info, tip)
	}

	fn get_priority(
		len: usize,
		info: &DispatchInfoOf<T::RuntimeCall>,
		final_fee: BalanceOf<T>,
	) -> TransactionPriority {
		let weight_saturation = T::BlockWeights::get().max_block / info.weight.ref_time().max(1);
		let max_block_length = *T::BlockLength::get().max.get(DispatchClass::Normal);
		let len_saturation = max_block_length as u64 / (len as u64).max(1);
		let coefficient: BalanceOf<T> = weight_saturation
			.ref_time()
			.min(len_saturation)
			.saturated_into::<BalanceOf<T>>();
		final_fee
			.saturating_mul(coefficient)
			.saturated_into::<TransactionPriority>()
	}

	#[allow(clippy::type_complexity)]
    fn withdraw_fee(
        &self,
        who: &T::AccountId,
        call: &T::RuntimeCall,
        info: &DispatchInfoOf<T::RuntimeCall>,
        len: usize,
	) -> Result<
		(
			BalanceOf<T>,
			T::AccountId,
			<<T as pallet_transaction_payment::Config>::OnChargeTransaction as pallet_transaction_payment::OnChargeTransaction<T>>::LiquidityInfo,
		),
		TransactionValidityError,
	>{
		let tip = self.0;
		let fee = Self::traditional_fee(len, info, tip);

		// Determine who is paying transaction fee based on ecnomic model
		// Parse call to extract collection ID and access collection sponsor
		let sponsor = T::SponsorshipHandler::get_sponsor(who, call);
		let who_pays_fee = sponsor.unwrap_or_else(|| who.clone());

		let liquidity_info = <<T as pallet_transaction_payment::Config>::OnChargeTransaction as pallet_transaction_payment::OnChargeTransaction<T>>::withdraw_fee(&who_pays_fee, call, info, fee, tip)?;

		Ok((fee, who_pays_fee, liquidity_info))
	}
}

impl<T: Config + Send + Sync + TypeInfo> SignedExtension for ChargeTransactionPayment<T>
where
	BalanceOf<T>: Send + Sync + From<u64> + FixedPointOperand,
	T::RuntimeCall: Dispatchable<Info = DispatchInfo, PostInfo = PostDispatchInfo>,
{
	const IDENTIFIER: &'static str = "ChargeTransactionPayment";
	type AccountId = T::AccountId;
	type Call = T::RuntimeCall;
	type AdditionalSigned = ();
	type Pre = (
        // tip
        BalanceOf<T>,
        // who pays fee
        Self::AccountId,
		// imbalance resulting from withdrawing the fee
		<<T as pallet_transaction_payment::Config>::OnChargeTransaction as pallet_transaction_payment::OnChargeTransaction<T>>::LiquidityInfo,
    );
	fn additional_signed(&self) -> sp_std::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> TransactionValidity {
		let (fee, _, _) = self.withdraw_fee(who, call, info, len)?;
		Ok(ValidTransaction {
			priority: Self::get_priority(len, info, fee),
			..Default::default()
		})
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		call: &Self::Call,
		info: &DispatchInfoOf<Self::Call>,
		len: usize,
	) -> Result<Self::Pre, TransactionValidityError> {
		let (_fee, who_pays_fee, imbalance) = self.withdraw_fee(who, call, info, len)?;
		Ok((self.0, who_pays_fee, imbalance))
	}

	fn post_dispatch(
		pre: Option<Self::Pre>,
		info: &DispatchInfoOf<Self::Call>,
		post_info: &PostDispatchInfoOf<Self::Call>,
		len: usize,
		_result: &DispatchResult,
	) -> Result<(), TransactionValidityError> {
		if let Some((tip, who_pays_fee, imbalance)) = pre {
			let actual_fee = pallet_transaction_payment::Pallet::<T>::compute_actual_fee(
				len as u32, info, post_info, tip,
			);
			<T as pallet_transaction_payment::Config>::OnChargeTransaction::correct_and_deposit_fee(
				&who_pays_fee, info, post_info, actual_fee, tip, imbalance,
			)?;
		}
		Ok(())
	}
}

/// Copy of CheckNonce from frame-system, except for removed
/// providers/consumers check, added in https://github.com/paritytech/polkadot-sdk/pull/1578.
/// TODO: Make this check configurable for the upstream CheckNonce/remove as it gets removed/made configurable in
/// upstream (Looks like it is planned: https://github.com/paritytech/polkadot-sdk/pull/1578#issuecomment-1754928101)
#[derive(Encode, Decode, Clone, Eq, PartialEq, TypeInfo)]
#[scale_info(skip_type_params(T))]
pub struct CheckNonce<T: Config>(#[codec(compact)] pub T::Nonce);

impl<T: Config> CheckNonce<T> {
	/// utility constructor. Used only in client/factory code.
	pub fn from(nonce: T::Nonce) -> Self {
		Self(nonce)
	}
}

impl<T: Config> sp_std::fmt::Debug for CheckNonce<T> {
	#[cfg(feature = "std")]
	fn fmt(&self, f: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		write!(f, "CheckNonce({})", self.0)
	}

	#[cfg(not(feature = "std"))]
	fn fmt(&self, _: &mut sp_std::fmt::Formatter) -> sp_std::fmt::Result {
		Ok(())
	}
}

impl<T: Config> SignedExtension for CheckNonce<T>
where
	T::RuntimeCall: Dispatchable<Info = DispatchInfo>,
{
	type AccountId = T::AccountId;
	type Call = T::RuntimeCall;
	type AdditionalSigned = ();
	type Pre = ();
	const IDENTIFIER: &'static str = "CheckNonce";

	fn additional_signed(&self) -> sp_std::result::Result<(), TransactionValidityError> {
		Ok(())
	}

	fn pre_dispatch(
		self,
		who: &Self::AccountId,
		_call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> Result<(), TransactionValidityError> {
		let mut account = frame_system::Account::<T>::get(who);
		// if account.providers.is_zero() && account.sufficients.is_zero() {
		// 	// Nonce storage not paid for
		// 	return Err(InvalidTransaction::Payment.into());
		// }
		if self.0 != account.nonce {
			return Err(if self.0 < account.nonce {
				InvalidTransaction::Stale
			} else {
				InvalidTransaction::Future
			}
			.into());
		}
		account.nonce += T::Nonce::one();
		frame_system::Account::<T>::insert(who, account);
		Ok(())
	}

	fn validate(
		&self,
		who: &Self::AccountId,
		_call: &Self::Call,
		_info: &DispatchInfoOf<Self::Call>,
		_len: usize,
	) -> TransactionValidity {
		let account = frame_system::Account::<T>::get(who);
		// if account.providers.is_zero() && account.sufficients.is_zero() {
		// 	// Nonce storage not paid for
		// 	return InvalidTransaction::Payment.into();
		// }
		if self.0 < account.nonce {
			return InvalidTransaction::Stale.into();
		}

		let provides = vec![Encode::encode(&(who, self.0))];
		let requires = if account.nonce < self.0 {
			vec![Encode::encode(&(who, self.0 - One::one()))]
		} else {
			vec![]
		};

		Ok(ValidTransaction {
			priority: 0,
			requires,
			provides,
			longevity: TransactionLongevity::max_value(),
			propagate: true,
		})
	}
}
