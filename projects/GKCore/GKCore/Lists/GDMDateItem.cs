/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Options;

namespace GKCore.Lists
{
    /// <summary>
    /// This class is wrapper for the GDM dates to a ListView items.
    /// </summary>
    public class GDMDateItem : IComparable, IConvertible
    {
        private readonly GDMCustomDate fDate;

        public GDMDateItem(GDMCustomDate date)
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
                strVal = fDate.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
            }

            return strVal;
        }

        public int CompareTo(object obj)
        {
            if (obj is GDMDateItem otherItem) {
                var cv1 = fDate;
                var cv2 = otherItem.fDate;

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
            return -1;
        }

        #region IConvertible implementation (this is necessary for EtoFw)

        TypeCode IConvertible.GetTypeCode()
        {
            return TypeCode.Object;
        }

        string IConvertible.ToString(IFormatProvider provider)
        {
            return ToString();
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

        object IConvertible.ToType(Type conversionType, IFormatProvider provider)
        {
            throw new NotImplementedException();
        }

        #endregion
    }
}
