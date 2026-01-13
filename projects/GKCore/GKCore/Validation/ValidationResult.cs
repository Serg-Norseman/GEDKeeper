/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;

namespace GKCore.Validation
{
    public sealed class ValidationResult
    {
        public static readonly ValidationResult Empty = new ValidationResult();

        public IList<ValidationMessage> Messages { get; internal set; }

        public bool Valid { get; private set; }


        public ValidationResult()
        {
            Messages = new List<ValidationMessage>();
            Valid = true;
        }

        /// <summary>
        /// Adds the error.
        /// </summary>
        public void AddError(string errorMessage)
        {
            Valid = false;
            Messages.Add(new ValidationMessage { Message = errorMessage });
        }

        /// <summary>
        /// Adds the warning.
        /// </summary>
        public void AddWarning(string warningMessage)
        {
            // No need to mark collection dirty since warnings never generate validation changes
            Messages.Add(new ValidationMessage { Message = warningMessage, Warning = true });
        }
    }
}
