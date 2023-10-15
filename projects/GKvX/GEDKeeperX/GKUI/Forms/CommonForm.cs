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
using GKCore;
using GKCore.Design;
using GKCore.Interfaces;
using GKUI.Platform;
using GKUI.Themes;
using Xamarin.Forms;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : ContentPage, IView, IThemedView
    {
        private readonly ControlsManager fControlsManager;

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

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControl<T>(control);
        }

        public object GetControl(string controlName)
        {
            return FindByName(controlName);
        }

        public void Close()
        {
            //base.Close();
        }

        public void Dispose()
        {
        }

        /*protected override void OnAppearing()
        {
            base.OnAppearing();
            ApplyTheme();
        }*/

        public virtual void ApplyTheme()
        {
            if (AppHost.Instance != null) {
                AppHost.Instance.ApplyTheme(this);
            }
        }

        public virtual bool SkipTheme(IDisposable component)
        {
            return false;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonWindow : CommonForm, IWindow
    {
        public virtual void Show(bool showInTaskbar)
        {
            XFAppHost.GetMainPage().Navigate(this);
        }

        public virtual void SetLocale()
        {
        }

        /*protected override void OnAppearing()
        {
            AppHost.Instance.LoadWindow(this);
            base.OnAppearing();
        }

        protected override void OnDisappearing()
        {
            AppHost.Instance.CloseWindow(this);
            base.OnDisappearing();
        }*/
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonWindow<TView, TController> : CommonWindow
        where TView : IView
        where TController : FormController<TView>
    {
        protected TController fController;


        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
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

        public virtual bool ShowModalX(IView owner)
        {
            XFAppHost.GetMainPage().Navigate(this);
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

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }




    /// <summary>
    /// 
    /// </summary>
    public class XCTDialog : Xamarin.CommunityToolkit.UI.Views.Popup, ICommonDialog
    {
        private readonly ControlsManager fControlsManager;

        #region View Interface

        public string Title
        {
            get { return string.Empty; }
            set {  }
        }

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        #endregion

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

        public XCTDialog()
        {
            base.IsLightDismissEnabled = false;

            fControlsManager = new ControlsManager(this);
        }

        public void Dispose()
        {
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

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControl<T>(control);
        }

        public object GetControl(string controlName)
        {
            return FindByName(controlName);
        }

        public virtual bool ShowModalX(IView owner)
        {
            //XFAppHost.GetMainPage().NavigateEx(this);
            return false;//(ShowModal((Control)owner) == DialogResult.Ok);
        }

        public void Close()
        {
            //base.Close();
        }

        /*public void Close(DialogResult dialogResult)
        {
            fResult = dialogResult;
            if (fResult != DialogResult.None) {
                Close();
            }
        }*/

        public virtual void ApplyTheme()
        {
            /*if (AppHost.Instance != null) {
                AppHost.Instance.ApplyTheme(this);
            }*/
        }

        public virtual bool SkipTheme(IDisposable component)
        {
            return false;
        }

        protected async virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            //Close(DialogResult.Cancel);
            //await Navigation.PopAsync();
        }

        protected async virtual void CancelClickHandler(object sender, EventArgs e)
        {
            //Close(DialogResult.Cancel);
            //await Navigation.PopAsync();
            //base.Dismiss(null);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class XCTDialog<TView, TController> : XCTDialog
        where TView : IView
        where TController : DialogController<TView>
    {
        protected TController fController;

        protected async override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                //fController.Accept();
                //await Navigation.PopAsync();
                //DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("XCTDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected async override void CancelClickHandler(object sender, EventArgs e)
        {
            try {
                base.Dismiss(null);

                //fController.Cancel();
                //await Navigation.PopAsync();
                //DialogResult = fController.Cancel() ? DialogResult.Cancel : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("XCTDialog<>.CancelClickHandler()", ex);
            }
        }

        /*protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }*/

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
