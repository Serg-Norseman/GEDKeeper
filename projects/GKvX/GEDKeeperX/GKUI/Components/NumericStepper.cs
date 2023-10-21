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

using Xamarin.Forms;

namespace GKUI.Components
{
    public class NumericStepper : ContentView
    {
        private readonly Entry fEntry;
        private readonly Stepper fStepper;
        private bool fInternalHandler;


        public int Increment
        {
            get { return (int)fStepper.Increment; }
            set { fStepper.Increment = value; }
        }

        public int MinValue
        {
            get { return (int)fStepper.Minimum; }
            set { fStepper.Minimum = value; }
        }

        public int MaxValue
        {
            get { return (int)fStepper.Maximum; }
            set { fStepper.Maximum = value; }
        }

        public int Value
        {
            get { return (int)fStepper.Value; }
            set { fStepper.Value = value; }
        }


        public NumericStepper()
        {
            fEntry = new Entry();
            fEntry.HorizontalOptions = LayoutOptions.StartAndExpand;
            fEntry.TextChanged += OnTextChanged;
            fEntry.Keyboard = Keyboard.Numeric;
            fEntry.WidthRequest = 60;
            fEntry.HorizontalTextAlignment = TextAlignment.End;

            fStepper = new Stepper();
            fStepper.HorizontalOptions = LayoutOptions.End;
            fStepper.ValueChanged += OnValueChanged;

            Content = new StackLayout() {
                Orientation = StackOrientation.Horizontal,
                Spacing = 0,
                Children = { fEntry, fStepper }
            };

            fInternalHandler = false;
        }

        private void OnTextChanged(object sender, TextChangedEventArgs e)
        {
            if (!fInternalHandler && int.TryParse(e.NewTextValue, out int val)) {
                fInternalHandler = true;
                fStepper.Value = val;
                fInternalHandler = false;
            }
        }

        private void OnValueChanged(object sender, ValueChangedEventArgs e)
        {
            if (!fInternalHandler) {
                fInternalHandler = true;
                fEntry.Text = ((int)e.NewValue).ToString();
                fInternalHandler = false;
            }
        }
    }
}
