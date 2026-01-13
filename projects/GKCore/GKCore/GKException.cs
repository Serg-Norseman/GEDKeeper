/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore
{
    /// <summary>
    /// 
    /// </summary>
    public class GKException : Exception
    {
        public GKException(string message)
            : base(message)
        {
        }
    }

    public class MediaFileNotFoundException : GKException
    {
        public MediaFileNotFoundException(string fileName)
            : base(string.Format("Media file '{0}' not found", fileName))
        {
        }
    }
}
