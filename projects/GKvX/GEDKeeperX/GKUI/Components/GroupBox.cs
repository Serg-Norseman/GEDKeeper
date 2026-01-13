/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
