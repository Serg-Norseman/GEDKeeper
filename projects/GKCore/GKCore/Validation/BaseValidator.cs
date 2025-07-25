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

using System;

namespace GKCore.Validation
{
    public abstract class BaseValidator<T> : IValidator<T>
    {
        public abstract ValidationResult Validate(T obj, bool suppressWarnings = false);

        protected static bool CheckMaximumLength(string value, int maxLength)
        {
            return string.IsNullOrEmpty(value) || value.Length <= maxLength;
        }

        protected static bool CheckNotEmpty(string value)
        {
            return !string.IsNullOrEmpty(value);
        }

        protected static bool CheckInclusiveBetween<V>(V value, V min, V max) where V : IComparable<V>
        {
            return value.CompareTo(min) >= 0 && value.CompareTo(max) <= 0;
        }

        protected static bool CheckExclusiveBetween<V>(V value, V min, V max) where V : IComparable<V>
        {
            return value.CompareTo(min) > 0 && value.CompareTo(max) < 0;
        }
    }
}
