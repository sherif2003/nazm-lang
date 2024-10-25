use derive_more::{From, Into};
use std::{collections::HashMap, hash::Hash, marker::PhantomData, usize};
pub use typed_index_collections;
use typed_index_collections::TiVec;

#[macro_export]
macro_rules! new_data_pool_key {
    ($name: ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default, Ord, PartialOrd, From, Into)]
        pub struct $name(usize);
    };
}

new_data_pool_key! { IdKey }

new_data_pool_key! { StrKey }

pub type IdPoolBuilder = DataPoolBuilder<IdKey, String>;

pub type StrPoolBuilder = DataPoolBuilder<StrKey, String>;

pub struct DataPoolBuilder<K, V>
where
    K: From<usize> + Into<usize>,
    V: Eq + Hash + Clone,
{
    map: HashMap<V, usize>,
    phantom_data: PhantomData<K>,
}

impl<K, V> DataPoolBuilder<K, V>
where
    K: From<usize> + Into<usize>,
    V: Eq + Hash + Clone,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            phantom_data: PhantomData,
        }
    }

    pub fn get_key(&mut self, val: &V) -> K {
        match self.map.get(val) {
            Some(index) => (*index).into(),
            None => {
                let index = self.map.len();
                self.map.insert(val.clone(), index);
                index.into()
            }
        }
    }

    pub fn build(self) -> TiVec<K, V> {
        let len = self.map.len();

        let mut data = TiVec::with_capacity(len);

        let ptr: *mut V = data.as_mut_ptr();

        for (key, index) in self.map {
            unsafe {
                ptr.add(index).write(key);
            }
        }

        unsafe {
            data.set_len(len);
        }

        data
    }
}

pub type IdPool = TiVec<IdKey, String>;

pub type StrPool = TiVec<StrKey, String>;

impl IdKey {
    pub const UNIT: Self = Self(0);
    pub const MAIN: Self = Self(1);
    pub const IMPLICIT_LAMBDA_PARAM: Self = Self(2);
}
