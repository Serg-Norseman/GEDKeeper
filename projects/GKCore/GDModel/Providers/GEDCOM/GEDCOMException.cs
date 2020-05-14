/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// This hierarchy of exception classes should only serve the needs of error handling in the GEDCOM format.
    /// </summary>
    public class GEDCOMException : GDMException
    {
        public GEDCOMException(string message) : base(message)
        {
        }

        public GEDCOMException(string message, Exception innerException)
            : base(message, innerException)
        {
        }
    }


    public class GEDCOMEmptyFileException : GEDCOMException
    {
        public GEDCOMEmptyFileException()
            : base("GEDCOM file is empty")
        {
        }
    }


    public class GEDCOMBlobDecodeException : GEDCOMException
    {
        public GEDCOMBlobDecodeException()
            : base("Blob decoding error")
        {
        }
    }


    public class GEDCOMIntDateException : GEDCOMException
    {
        public GEDCOMIntDateException(string intDate)
            : base(string.Format("The interpreted date '{0}' doesn't start with a valid ident", intDate))
        {
        }
    }


    public class GEDCOMRangeDateException : GEDCOMException
    {
        public GEDCOMRangeDateException(string rangeDate)
            : base(string.Format("The range date '{0}' doesn't contain 'and' token", rangeDate))
        {
        }
    }


    public class GEDCOMInvalidFormatException : GEDCOMException
    {
        public GEDCOMInvalidFormatException(string message) : base(message)
        {
        }

        public GEDCOMInvalidFormatException(string message, Exception innerException)
            : base(message, innerException)
        {
        }
    }


    public class GEDCOMParserException : GEDCOMException
    {
        public GEDCOMParserException(string message) : base(message)
        {
        }
    }
}
