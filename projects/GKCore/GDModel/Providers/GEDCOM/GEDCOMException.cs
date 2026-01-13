/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GDModel.Providers.GEDCOM
{
    /// <summary>
    /// This hierarchy of exception classes should only serve the needs of error handling in the GEDCOM format.
    /// Processing the GEDCOM format is one part of the Genealogical Data Model (GDM).
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
