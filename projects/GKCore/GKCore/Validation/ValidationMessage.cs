/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Validation
{
    public sealed class ValidationMessage
    {
        public string Message { get; internal set; }
        public bool Warning { get; internal set; }

        /// <summary>
        /// Only allow creation internally or by ValidationResult class.
        /// </summary>
        internal ValidationMessage()
        {
        }
    }
}
