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

using Avalonia.Controls;
using Avalonia.Interactivity;
using BSLib.Design.MVP;
using GKCore.Interfaces;
using GKCore.MVP;
using IControl = BSLib.Design.MVP.IControl;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Window, IView
    {
        private readonly ControlsManager fControlsManager;
        private string fTitle;

        #region View Interface

        public string Title
        {
            get { return fTitle; }
            set { fTitle = value; }
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

            base.SizeToContent = SizeToContent.WidthAndHeight;
        }

        public void SetToolTip(Control component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                // not supported
            }
        }

        public void Activate()
        {
            base.Focus();
        }

        public void Close()
        {
            base.Close();
        }

        public void Dispose()
        {
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControlHandler<T>(control);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonWindow : CommonForm, IWindow
    {
        public CommonWindow()
        {
        }

        public virtual void Show(bool showInTaskbar)
        {
            //ShowInTaskbar = showInTaskbar;
            //Show();
        }

        public virtual void SetLang()
        {
        }
    }


    public enum DialogResult
    {
        None,
        Ok,
        Cancel,
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonDialog : CommonForm, ICommonDialog
    {
        private DialogResult fResult;

        public DialogResult DialogResult
        {
            get { return fResult; }
            set {
                if (fResult != value) {
                    fResult = value;
                    if (value != DialogResult.None) {
                        Close();
                    }
                }
            }
        }

        public CommonDialog()
        {
            base.CanResize = false;
        }

        public virtual bool ShowModalX(object owner)
        {
            return false;//(ShowModal((Control)owner) == DialogResult.Ok);
        }

        public void Close(DialogResult dialogResult)
        {
            fResult = dialogResult;
            if (fResult != DialogResult.None) {
                Close();
            }
        }

        protected virtual void CancelClickHandler(object sender, RoutedEventArgs e)
        {
            Close(DialogResult.Cancel);
        }

        public void SetPredefProperties(int width, int height, bool fontPreset = true)
        {
            //UIHelper.SetPredefProperties(this, width, height, fontPreset);
        }
    }
}
