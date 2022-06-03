/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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
using BSLib.Design.MVP;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP;
using Xamarin.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : ContentPage, IView
    {
        protected readonly ControlsManager fControlsManager;

        #region View Interface

        public string Title
        {
            get { return base.Title; }
            set { base.Title = value; }
        }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        #endregion

        public CommonForm()
        {
            fControlsManager = new ControlsManager(this);
        }

        public void SetToolTip(object component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                // not supported
            }
        }

        public void Activate()
        {
            Focus();
        }

        public void Close()
        {
            //base.Close();
        }

        public void Dispose()
        {
        }

        public object GetControl(string controlName)
        {
            return FindByName(controlName);
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControl<T>(control);
        }

        public virtual void SetLocale()
        {
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

        public CommonDialog()
        {
        }

        public virtual bool ShowModalX(object owner)
        {
            return false;//(ShowModal((Control)owner) == DialogResult.Ok);
        }

        /*public void Close(DialogResult dialogResult)
        {
            fResult = dialogResult;
            if (fResult != DialogResult.None) {
                Close();
            }
        }*/

        protected async virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            //Close(DialogResult.Cancel);
            await Navigation.PopAsync();
        }

        protected async virtual void CancelClickHandler(object sender, EventArgs e)
        {
            //Close(DialogResult.Cancel);
            await Navigation.PopAsync();
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonDialog<TView, TController> : CommonDialog
        where TView : IView
        where TController : DialogController<TView>
    {
        protected TController fController;

        protected async override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                fController.Accept();
                await Navigation.PopAsync();
                //DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected async override void CancelClickHandler(object sender, EventArgs e)
        {
            try {
                fController.Cancel();
                await Navigation.PopAsync();
                //DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.CancelClickHandler()", ex);
            }
        }

        /*protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }*/
    }
}
