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

namespace GKCommon.GEDCOM
{
    /// <summary>
    /// Base class for all GEDCOM objects like tags, and tree and xref replacer.
    /// </summary>
    public class GEDCOMObject : IDisposable
    {
        public const char GEDCOM_DELIMITER = ' ';
        public const char GEDCOM_YEAR_MODIFIER_SEPARATOR = '/';
        public const string GEDCOM_YEAR_BC = "B.C.";
        public const char GEDCOM_POINTER_DELIMITER = '@';
        public const string GEDCOM_NEWLINE = "\r\n";

        // deprecated
        //public const byte GEDCOMMaxPhoneNumbers = 3;
        //public const byte GEDCOMMaxEmailAddresses = 3;
        //public const byte GEDCOMMaxFaxNumbers = 3;
        //public const byte GEDCOMMaxWebPages = 3;
        //public const byte GEDCOMMaxLanguages = 3;

        private bool fDisposed;

        public object ExtData
        {
            get;
            set;
        }

        protected virtual void Dispose(bool disposing)
        {
        }

        public void Dispose()
        {
            if (!fDisposed)
            {
                Dispose(true /*called by user directly*/);
                fDisposed = true;
            }

            GC.SuppressFinalize(this);
        }

        ~GEDCOMObject()
        {
            Dispose(false /*not called by user directly*/);
        }
    }
}
