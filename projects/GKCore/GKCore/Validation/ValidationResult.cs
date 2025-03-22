/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
