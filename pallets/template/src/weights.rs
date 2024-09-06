use frame_support::{
	traits::Get,
	weights::{constants::RocksDbWeight, Weight},
};
use core::marker::PhantomData;

pub trait WeightInfo {
	fn do_something() -> Weight;
	fn cause_error() -> Weight;
}

pub struct TemplateWeight<T>(PhantomData<T>);
impl<T: frame_system::Config> WeightInfo for TemplateWeight<T> {
	fn do_something() -> Weight {
		Weight::from_parts(10_000, 0)
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}

	fn cause_error() -> Weight {
		Weight::from_parts(10_000, 0)
			.saturating_add(T::DbWeight::get().reads(1))
			.saturating_add(T::DbWeight::get().writes(1))
	}
}

impl WeightInfo for () {
	fn do_something() -> Weight {
		Weight::from_parts(10_000, 0)
			.saturating_add(RocksDbWeight::get().reads(1))
			.saturating_add(RocksDbWeight::get().writes(1))
	}

	fn cause_error() -> Weight {
		Weight::from_parts(10_000, 0)
			.saturating_add(RocksDbWeight::get().reads(1))
			.saturating_add(RocksDbWeight::get().writes(1))
	}
}