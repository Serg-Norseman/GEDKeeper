/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
