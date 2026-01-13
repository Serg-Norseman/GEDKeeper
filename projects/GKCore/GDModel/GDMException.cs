/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GDModel
{
    /// <summary>
    /// This hierarchy of exception classes should only serve the needs of error handling 
    /// in the Genealogical Data Model (GDM subsystem of GEDKeeper's Core).
    /// </summary>
    public class GDMException : Exception
    {
        public GDMException(string message) : base(message)
        {
        }

        public GDMException(string message, params object[] args) : base(string.Format(message, args))
        {
        }
    }


    public class GDMDateException : GDMException
    {
        public GDMDateException(string message) : base(message)
        {
        }

        public GDMDateException(string message, params object[] args) : base(message, args)
        {
        }
    }
}
