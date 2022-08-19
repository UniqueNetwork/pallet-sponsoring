use crate::{Call, Config};
use core::marker::PhantomData;
use frame_support::traits::IsSubType;
use up_sponsorship::SponsorshipHandler;
// use nft_data_structs::{
// 	TokenId, CollectionId, NFT_SPONSOR_TRANSFER_TIMEOUT, REFUNGIBLE_SPONSOR_TRANSFER_TIMEOUT,
// 	FUNGIBLE_SPONSOR_TRANSFER_TIMEOUT,
// };

pub struct NftSponsorshipHandler<T>(PhantomData<T>);

#[allow(unused_variables)]
impl<T: Config> NftSponsorshipHandler<T> {
	pub fn withdraw(
		who: &T::AccountId,
		something: &u32,
		who_will_pay: &T::AccountId,
	) -> Option<T::AccountId> {
		Some(who_will_pay.clone())
	}
}

impl<T, C, F> SponsorshipHandler<T::AccountId, C, F> for NftSponsorshipHandler<T>
where
	T: Config,
	C: IsSubType<Call<T>>,
{
	fn get_sponsor(who: &T::AccountId, call: &C, _fee_limit: &F) -> Option<T::AccountId> {
		match IsSubType::<Call<T>>::is_sub_type(call)? {
			Call::do_something { something, who_will_pay } =>
				Self::withdraw(who, something, who_will_pay),
			_ => None,
		}
	}
}
