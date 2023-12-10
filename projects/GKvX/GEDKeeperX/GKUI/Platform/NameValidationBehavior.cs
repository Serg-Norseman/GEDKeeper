/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using System.Linq;
using Xamarin.Forms;

namespace GKUI.Platform
{
    public class NameValidationBehavior : Behavior<Entry>
    {
        // Here we have added the characters we wanted to restrict while entering data.
        private const string SpecialCharacters = @"'/\%*‘;$£&#^@|?+=<>\""";

        protected override void OnAttachedTo(Entry entry)
        {
            entry.TextChanged += OnEntryTextChanged;
            base.OnAttachedTo(entry);
        }

        protected override void OnDetachingFrom(Entry entry)
        {
            entry.TextChanged -= OnEntryTextChanged;
            base.OnDetachingFrom(entry);
        }

        private static void OnEntryTextChanged(object sender, TextChangedEventArgs args)
        {
            string newText = args.NewTextValue;

            if (!string.IsNullOrWhiteSpace(newText)) {
                bool isValid = newText.ToCharArray().All(x => !SpecialCharacters.Contains(x));

                ((Entry)sender).Text = isValid ? newText : newText.Remove(newText.Length - 1);
            }
        }
    }
}
