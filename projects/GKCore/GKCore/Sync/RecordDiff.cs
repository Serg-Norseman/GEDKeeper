/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GDModel;
using GKCore.Utilities;

namespace GKCore.Sync
{
    #region Classes of differences

    public sealed class RecordDiff : DiffResult<GDMRecord>
    {
        public RecordDiff(GDMRecord obj1, GDMRecord obj2, DiffStatus status) : base(obj1, obj2, status)
        {
        }
    }


    public sealed class ValDiff : DiffResult<object>
    {
        public int ValType { get; private set; }
        public string DisplayItem1 { get; private set; }
        public string DisplayItem2 { get; private set; }

        public ValDiff(int valType, object obj1, string displayItem1, object obj2, string displayItem2, DiffStatus status) : base(obj1, obj2, status)
        {
            ValType = valType;
            DisplayItem1 = displayItem1;
            DisplayItem2 = displayItem2;
        }
    }


    public sealed class TagDiff : DiffResult<GDMTag>
    {
        public TagDiff(GDMTag obj1, GDMTag obj2, DiffStatus status) : base(obj1, obj2, status)
        {
        }
    }

    #endregion 

    #region Comparison classes

    internal class RecordComparer : IEqualityComparer<GDMRecord>
    {
        public bool Equals(GDMRecord x, GDMRecord y)
        {
            return x.UID == y.UID;
        }

        public int GetHashCode(GDMRecord obj)
        {
            return obj.GetHashCode();
        }
    }


    internal class EventComparer<T> : IEqualityComparer<T> where T : GDMCustomEvent
    {
        public bool Equals(T x, T y)
        {
            return x.Id == y.Id;
        }

        public int GetHashCode(T obj)
        {
            return obj.GetHashCode();
        }
    }


    internal class TagComparer<T> : IEqualityComparer<T> where T : GDMTag
    {
        public bool Equals(T x, T y)
        {
            return x.GetHashCode() == y.GetHashCode();
        }

        public int GetHashCode(T obj)
        {
            return obj.GetHashCode();
        }
    }


    internal class ValueTagComparer<T> : IEqualityComparer<T> where T : GDMValueTag
    {
        public bool Equals(T x, T y)
        {
            return x.StringValue == y.StringValue;
        }

        public int GetHashCode(T obj)
        {
            return obj.GetHashCode();
        }
    }


    internal class PointerComparer<T> : IEqualityComparer<T> where T : GDMPointer
    {
        public bool Equals(T x, T y)
        {
            return x.XRef == y.XRef;
        }

        public int GetHashCode(T obj)
        {
            return obj.GetHashCode();
        }
    }

    #endregion
}
