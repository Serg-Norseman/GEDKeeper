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
using GDModel;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// This class is wrapper for the GEDCOM dates to a ListView items.
    /// </summary>
    public class GEDCOMDateItem : IComparable, IConvertible
    {
        private readonly GDMCustomDate fDate;

        public GEDCOMDateItem(GDMCustomDate date)
        {
            fDate = date;
        }

        public override string ToString()
        {
            string strVal;

            if (fDate == null) {
                strVal = "";
            } else {
                GlobalOptions glob = GlobalOptions.Instance;
                strVal = fDate.GetDisplayStringExt(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
            }

            return strVal;
        }

        public int CompareTo(object obj)
        {
            GEDCOMDateItem otherItem = obj as GEDCOMDateItem;
            if (otherItem == null) {
                return -1;
            }

            IComparable cv1 = fDate;
            IComparable cv2 = otherItem.fDate;

            int compRes;
            if (cv1 != null && cv2 != null) {
                compRes = cv1.CompareTo(cv2);
            } else if (cv1 != null) {
                compRes = -1;
            } else if (cv2 != null) {
                compRes = 1;
            } else {
                compRes = 0;
            }
            return compRes;
        }

        #region IConvertible implementation (this is necessary for EtoFw)

        TypeCode IConvertible.GetTypeCode()
        {
            return TypeCode.Object;
        }

        bool IConvertible.ToBoolean(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        char IConvertible.ToChar(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        sbyte IConvertible.ToSByte(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        byte IConvertible.ToByte(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        short IConvertible.ToInt16(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        ushort IConvertible.ToUInt16(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        int IConvertible.ToInt32(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        uint IConvertible.ToUInt32(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        long IConvertible.ToInt64(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        ulong IConvertible.ToUInt64(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        float IConvertible.ToSingle(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        double IConvertible.ToDouble(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        decimal IConvertible.ToDecimal(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        DateTime IConvertible.ToDateTime(IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        string IConvertible.ToString(IFormatProvider provider)
        {
            return ToString();
        }

        object IConvertible.ToType(Type conversionType, IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        #endregion
    }
}
