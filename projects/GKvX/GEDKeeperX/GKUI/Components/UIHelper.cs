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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using BSLib;
using CarouselView.FormsPlugin.Abstractions;
using GDModel;
using GKCore;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Options;
using SkiaSharp;
using Xam.Plugin.TabView;
using Xamarin.Essentials;
using Xamarin.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Static functions only for UI implementation.
    /// </summary>
    public static class UIHelper
    {
        public static Rectangle Rt2Rt(ExtRect ert)
        {
            return new Rectangle(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect Rt2Rt(Rectangle ert)
        {
            return ExtRect.CreateBounds((int)ert.Left, (int)ert.Top, (int)ert.Width, (int)ert.Height);
        }

        public static SKRect Rt2SkRt(ExtRect ert)
        {
            return new SKRect(ert.Left, ert.Top, ert.GetWidth(), ert.GetHeight());
        }

        public static ExtRect SkRt2Rt(SKRect ert)
        {
            return ExtRect.CreateBounds((int)ert.Left, (int)ert.Top, (int)ert.Width, (int)ert.Height);
        }

        public static T GetSelectedTag<T>(this Picker picker)
        {
            var selectedItem = picker.SelectedItem as ComboItem<T>;
            return (selectedItem == null) ? default : selectedItem.Tag;
        }

        public static void SetSelectedTag<T>(this Picker picker, T tagValue, bool allowDefault = true)
        {
            foreach (object item in picker.Items) {
                var comboItem = item as ComboItem<T>;

                if (comboItem != null && Equals(comboItem.Tag, tagValue)) {
                    picker.SelectedItem = item;
                    return;
                }
            }

            if (allowDefault) {
                picker.SelectedIndex = 0;
            }
        }

        public static GKListView CreateRecordsView(ContentView parent, IBaseContext baseContext, GDMRecordType recType, bool simpleList)
        {
            if (parent == null)
                throw new ArgumentNullException("parent");

            if (baseContext == null)
                throw new ArgumentNullException("baseContext");

            GKListView recView = new GKListView();
            recView.ListMan = RecordsListModel<GDMRecord>.Create(baseContext, recType, simpleList);
            parent.Content = recView;

            return recView;
        }

        public static IColor ConvertColor(Color color)
        {
            return new ColorHandler(color);
        }

        public static Color ConvertColor(IColor color)
        {
            return ((ColorHandler)color).Handle;
        }

        public static Color Darker(Color color, float fraction)
        {
            int rgb = (int)(ColorExtensions.ToUInt(color) & 0xffffff);
            int newColor = GfxHelper.Darker(rgb, fraction);
            return Color.FromUint((uint)newColor);
        }

        public static Color Lighter(Color color, float fraction)
        {
            int rgb = (int)(ColorExtensions.ToUInt(color) & 0xffffff);
            int newColor = GfxHelper.Lighter(rgb, fraction);
            return Color.FromUint((uint)newColor);
        }

        public static void ProcessName(object sender)
        {
            var tb = (sender as Entry);
            if (tb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                tb.Text = GKUtils.UniformName(tb.Text);
            }

            var cmb = (sender as GKComboBox);
            if (cmb != null && GlobalOptions.Instance.FirstCapitalLetterInNames) {
                cmb.Text = GKUtils.UniformName(cmb.Text);
            }
        }

        public static string[] Convert(string text)
        {
            var strList = new StringList(text);
            return strList.ToArray();
        }

        public static string Convert(string[] lines)
        {
            StringBuilder strBuilder = new StringBuilder();
            foreach (var line in lines) {
                if (strBuilder.Length > 0) {
                    strBuilder.Append(Environment.NewLine);
                }
                strBuilder.Append(line);
            }
            return strBuilder.ToString();
        }

        public static ImageSource LoadResourceImage(string resName)
        {
            if (string.IsNullOrEmpty(resName)) {
                return null;
            }

            var dens = DeviceDisplay.MainDisplayInfo.Density;
            if (dens >= 2) {
                string fileName = Path.GetFileNameWithoutExtension(resName);
                string newName = string.Format("{0}@2x.png", fileName);
                var asm = typeof(UIHelper).Assembly;
                if (asm.GetManifestResourceInfo(newName) != null) {
                    return ImageSource.FromResource(newName, asm);
                }
            }

            return ImageSource.FromResource(resName, typeof(GKUtils).Assembly);
        }

        public static ImageSource LoadResourceImage(Type baseType, string resName)
        {
            return ImageSource.FromResource(resName, baseType.Assembly);
        }

        public static async Task<T> SelectItem<T>(object owner, IEnumerable<T> items)
        {
            var page = owner as Page;
            if (page == null) return default(T);

            var title = GKData.APP_TITLE;
            string[] strItems = items.Select(x => x.ToString()).ToArray();
            string action = await page.DisplayActionSheet(title, LangMan.LS(LSID.DlgCancel), null, strItems);
            return string.IsNullOrEmpty(action) ? default(T) : items.FirstOrDefault(x => x.ToString() == action);
        }

        public static Page GetParentPage(Element element)
        {
            if (element == null) return null;

            while (element != null && !(element is Page)) {
                element = element.Parent;
            }
            return element as Page;
        }

        public static object InvokeMethod<T>(T obj, string methodName, params object[] args)
        {
            var method = typeof(T).GetTypeInfo().GetDeclaredMethod(methodName);
            return method?.Invoke(obj, args);
        }

        public static void UnsetAnimateTransition(TabViewControl tabView)
        {
            // TabViewControl._carouselView.AnimateTransition = false (!)
            var _carouselView = typeof(TabViewControl).GetField("_carouselView", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(tabView) as CarouselViewControl;
            if (_carouselView != null) {
                _carouselView.AnimateTransition = false;
            }
        }

        public static void ResetTabViewLayout(TabViewControl tabView)
        {
            // HACK: Content of TabViewControl disappears when switching to menu+start screen and then returning back.
            tabView.Layout((tabView.Parent as View).Bounds);
        }
    }
}
