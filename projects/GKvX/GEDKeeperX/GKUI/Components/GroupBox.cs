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

using System.Collections.Generic;
using Xamarin.Forms;
using Xamarin.Forms.Xaml;

namespace GKUI.Components
{
    [ContentProperty("Contents")]
    [XamlCompilation(XamlCompilationOptions.Compile)]
    public class GroupBox : ContentView
    {
        private readonly StackLayout fContentLayout;
        private readonly Label fTitle;

        public IList<View> Contents { get => fContentLayout.Children; }

        public string Text
        {
            get { return fTitle.Text; }
            set { fTitle.Text = value; }
        }

        public GroupBox()
        {
            fTitle = new Label();

            fContentLayout = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                HorizontalOptions = LayoutOptions.FillAndExpand,
                VerticalOptions = LayoutOptions.FillAndExpand,
                Padding = 0,
                Spacing = 0
            };

            var content = new StackLayout() {
                Orientation = StackOrientation.Vertical,
                Padding = 0,
                Spacing = 0,
                Children = {
                    fTitle,
                    new Frame() {
                        Padding = 8,
                        Content = fContentLayout
                    }
                }
            };

            base.Content = content;
        }
    }
}
