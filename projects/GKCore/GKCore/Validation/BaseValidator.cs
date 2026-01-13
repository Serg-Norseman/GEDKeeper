/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
