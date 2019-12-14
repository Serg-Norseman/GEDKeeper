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

namespace GDModel
{
#pragma warning disable CA2229 // Implement serialization constructors
    /// <summary>
    /// Base application exception class
    /// </summary>
    [Serializable]
    public class GDMException : Exception
    {
        public GDMException(string message) : base(message)
        {
        }

        public GDMException()
        {
        }

        public GDMException(string message, Exception innerException) : base(message, innerException)
        {
        }

    }

    [Serializable]
    public class GDMBlobDecodeException : GDMException
    {
        public GDMBlobDecodeException() : base("Blob decoding error catched")
        {
        }
    }

    #region DGMDateExeptions
    [Serializable]
    public class GDMDateException : GDMException
    {
        public GDMDateException(string message) : base(message)
        {
        }
    }

    [Serializable]
    public class GDMDateIdentException : GDMDateException
    {
        public GDMDateIdentException(string ident) : base(string.Format("The interpreted date '{0}' doesn't start with a valid ident", ident))
        {
        }
    }

    [Serializable]
    public class GDMDateNotContainsTokenException : GDMDateException
    {
        public GDMDateNotContainsTokenException(string token) : base(string.Format("The range date '{0}' doesn't contain 'and' token",token))
        {
        }
    }
    #endregion

    /// <summary>
    /// Invalid data format exception
    /// </summary>
    [Serializable]

    public class GDMInvalidFormatException : GDMException

    {
        public GDMInvalidFormatException(int lineNumber) : base(string.Format("The string {0} doesn't start with a valid number", lineNumber))
        {

        }
    }

    [Serializable]
    public class GEDCOMEmptyFileException : GDMException
    {
        public GEDCOMEmptyFileException() : base("GEDCOM file is empty")
        {
        }
    }

#pragma warning restore CA2229 // Implement serialization constructors
}
