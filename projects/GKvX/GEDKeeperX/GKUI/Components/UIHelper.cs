/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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

using GKCore;
using System;
using Xamarin.Forms;

namespace GKUI.Components
{
    public static class UIHelper
    {
        public static StackLayout CreateHStackLayout(params View[] items)
        {
            return CreateStackLayout(StackOrientation.Horizontal, 0, 10, items);
        }

        public static StackLayout CreateVStackLayout(params View[] items)
        {
            return CreateStackLayout(StackOrientation.Vertical, 0, 10, items);
        }

        public static StackLayout CreateStackLayout(StackOrientation orientation,
                                                    int padding, int spacing,
                                                    params View[] items)
        {
            var res = new StackLayout();
            res.Orientation = orientation;
            res.Padding = new Thickness(padding);
            res.Spacing = spacing;
            res.HorizontalOptions = LayoutOptions.Center;
            foreach (var itm in items)
            {
                res.Children.Add(itm);
            }
            return res;
        }

        public static Button CreateDialogButton(string title, EventHandler clickHandler)
        {
            var btn = new Button();
            btn.ContentLayout = new Button.ButtonContentLayout(Button.ButtonContentLayout.ImagePosition.Left, 10);
            btn.VerticalOptions = LayoutOptions.CenterAndExpand;
            btn.HorizontalOptions = LayoutOptions.Center;
            btn.Text = title;
            btn.Clicked += clickHandler;
            return btn;
        }

        public static ImageSource LoadResourceImage(string resName)
        {
            //return ImageSource.FromStream( GKUtils.LoadResourceStream(resName));
            return ImageSource.FromResource(resName, typeof(GKUtils).Assembly);
        }

        public static ImageSource LoadResourceImage(Type baseType, string resName)
        {
            //return new FileImageSource(GKUtils.LoadResourceStream(baseType, resName));
            return ImageSource.FromResource(resName, baseType.Assembly);
        }
    }
}
