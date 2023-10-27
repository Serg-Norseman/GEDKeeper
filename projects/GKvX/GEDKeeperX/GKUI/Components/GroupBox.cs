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
    public class GroupBox : ContentView
    {
        private readonly Frame fContent;
        private readonly Label fTitle;

        public new View Content
        {
            get { return fContent.Content; }
            set { fContent.Content = value; }
        }

        public string Text
        {
            get { return fTitle.Text; }
            set {
                fTitle.Text = value;
                ForceLayout();
            }
        }

        public GroupBox()
        {
            fTitle = new Label() {
                //HorizontalOptions = LayoutOptions.FillAndExpand,
                //VerticalOptions = LayoutOptions.Start,
                Text = "GroupBox",
            };

            fContent = new Frame() {
                //HorizontalOptions = LayoutOptions.FillAndExpand,
                //VerticalOptions = LayoutOptions.FillAndExpand,
                //Padding = 4
            };

            var contentGrid = new Grid() {
                //Padding = new Thickness(4),
                //HorizontalOptions = LayoutOptions.FillAndExpand,
                //VerticalOptions = LayoutOptions.FillAndExpand,
                //Orientation = StackOrientation.Vertical,
                //Children = { fTitle, fContent }
            };
            contentGrid.Children.Add(fTitle, 0, 0);
            contentGrid.Children.Add(fContent, 0, 1);

            base.Content = contentGrid;
        }
    }
}
