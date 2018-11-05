/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using Windows.UI.Xaml.Controls;
using GKCore.Interfaces;
using GKCore.MVP;
using Windows.UI.Xaml;
//using GKUI.Components;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Page, IView
    {
        protected readonly ControlsManager fControlsManager;
        private string fCaption;

        #region View Interface

        public string Caption
        {
            get { return fCaption; }
            set { fCaption = value; }
        }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        #endregion

        public CommonForm()
        {
            fControlsManager = new ControlsManager();
        }

        public void SetToolTip(UIElement component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                // not supported
            }
        }

        public void Activate()
        {
            // not supported
        }

        public void Close()
        {
        }

        public void Dispose()
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class CommonWindow : CommonForm, IWindow
    {
        public virtual void Show(bool showInTaskbar)
        {
            //ShowInTaskbar = showInTaskbar;
            //Show();
        }

        public virtual void SetLang()
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonDialog : CommonForm, ICommonDialog
    {
        /*public DialogResult DialogResult
        {
            get { return base.Result; }
            set {
                if (base.Result != value) {
                    base.Result = value;
                    if (value != DialogResult.None) {
                        Close();
                    }
                }
            }
        }*/

        public CommonDialog() : base()
        {
        }

        public virtual bool ShowModalX(object owner)
        {
            return false;//(ShowModal((Control)owner) == DialogResult.Ok);
        }

        protected async virtual void CancelClickHandler(object sender, RoutedEventArgs e)
        {
            //Close(DialogResult.Cancel);
            this.Frame.GoBack();
        }

        public void SetPredefProperties(int width, int height, bool fontPreset = true)
        {
            //UIHelper.SetPredefProperties(this, width, height, fontPreset);
        }
    }
}
