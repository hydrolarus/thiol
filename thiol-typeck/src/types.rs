// SPDX-FileCopyrightText: 2021 The thiol developers
//
// SPDX-License-Identifier: EUPL-1.2

use thiol_hir::Identifier;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(pub(crate) usize);

impl TypeId {
    #[doc(hidden)]
    pub fn as_usize(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub args: Vec<(Identifier, TypeId)>,
    pub ret: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Bool,
    Int,
    UInt,
    Float,
    Double,

    BoolVec {
        components: VecSize,
    },
    IntVec {
        components: VecSize,
        vtype: VecType,
        space: Option<Identifier>,
    },
    UIntVec {
        components: VecSize,
        vtype: VecType,
        space: Option<Identifier>,
    },

    FloatVec {
        components: VecSize,
        vtype: VecType,
        space: Option<Identifier>,
    },
    DoubleVec {
        components: VecSize,
        vtype: VecType,
        space: Option<Identifier>,
    },

    FloatMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Identifier, Identifier)>,
    },
    DoubleMat {
        cols: VecSize,
        rows: VecSize,
        transform: Option<(Identifier, Identifier)>,
    },

    Array {
        base: TypeId,
        size: usize,
    },
    OpenArray {
        base: TypeId,
    },

    Record {
        fields: Vec<(Identifier, TypeId)>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VecType {
    Unknown,
    Point,
    Vector,
    Colour,
}

impl From<Option<thiol_hir::VecType>> for VecType {
    fn from(ty: Option<thiol_hir::VecType>) -> Self {
        match ty {
            Some(thiol_hir::VecType::Point) => Self::Point,
            Some(thiol_hir::VecType::Vector) => Self::Vector,
            Some(thiol_hir::VecType::Colour) => Self::Colour,
            None => Self::Unknown,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum VecSize {
    VS2,
    VS3,
    VS4,
}

impl From<thiol_hir::VecSize> for VecSize {
    fn from(s: thiol_hir::VecSize) -> Self {
        match s {
            thiol_hir::VecSize::VS2 => Self::VS2,
            thiol_hir::VecSize::VS3 => Self::VS3,
            thiol_hir::VecSize::VS4 => Self::VS4,
        }
    }
}
