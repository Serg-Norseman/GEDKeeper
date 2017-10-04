/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System;
using System.Text;

namespace GKCommon
{
    public struct EnumSet<T> : ICloneable where T : struct, IComparable, IFormattable
        #if !PCL
        , IConvertible
        #endif
    {
        private ulong fData;

        private EnumSet(ulong value)
        {
            fData = value;
        }

        public static EnumSet<T> Create(params T[] args)
        {
            EnumSet<T> result = new EnumSet<T>(0);
            result.Include(args);
            return result;
        }

        public void Include(params T[] e)
        {
            if (e == null) return;

            for (int i = 0; i < e.Length; i++) {
                Include(e[i]);
            }
        }

        public void Include(T elem)
        {
            byte idx = ((IConvertible)elem).ToByte(null);
            fData = (fData | (1u << idx));
        }

        public void Exclude(T elem)
        {
            byte idx = ((IConvertible)elem).ToByte(null);
            fData = (fData & (~(1u << idx)));
        }

        public bool Contains(T elem)
        {
            byte idx = ((IConvertible)elem).ToByte(null);
            return (fData & (1u << idx)) > 0u;
        }

        public bool ContainsAll(params T[] e)
        {
            if (e == null || e.Length == 0) return false;

            for (int i = 0; i < e.Length; i++) {
                if (!Contains(e[i])) {
                    return false;
                }
            }
            return true;
        }

        public bool HasIntersect(params T[] e)
        {
            if (e == null || e.Length == 0) return false;

            for (int i = 0; i < e.Length; i++) {
                if (Contains(e[i])) {
                    return true;
                }
            }
            return false;
        }

        public void Clear()
        {
            fData = 0;
        }

        public bool IsEmpty()
        {
            return (fData == 0);
        }

        public static bool operator ==(EnumSet<T> left, EnumSet<T> right)
        {
            return (left.fData == right.fData);
        }

        public static bool operator !=(EnumSet<T> left, EnumSet<T> right)
        {
            return (left.fData != right.fData);
        }

        public static EnumSet<T> operator +(EnumSet<T> left, EnumSet<T> right)
        {
            EnumSet<T> result = left;
            result.fData |= right.fData;
            return result;
        }

        public static EnumSet<T> operator -(EnumSet<T> left, EnumSet<T> right)
        {
            EnumSet<T> result = left;
            result.fData = result.fData & (~right.fData);
            return result;
        }

        public static EnumSet<T> operator *(EnumSet<T> left, EnumSet<T> right)
        {
            EnumSet<T> result = left;
            result.fData &= right.fData;
            return result;
        }

        public override string ToString()
        {
            ulong val = fData;
            ulong bt = 1;

            StringBuilder b = new StringBuilder(64);
            for (int i = 1; i <= 64; i++) {
                string sym = ((val & bt) > 0) ? "1" : "0";
                b.Insert(0, sym);
                bt = bt << 1;
            }

            return b.ToString();
        }

        public override int GetHashCode()
        {
            return fData.GetHashCode();
        }

        public override bool Equals(object obj)
        {
            if (!(obj is EnumSet<T>)) return false;

            EnumSet<T> setObj = (EnumSet<T>)obj;
            return (this == setObj);
        }

        // ICloneable
        public object Clone()
        {
            return new EnumSet<T>(fData);
        }
    }
}
