/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Threading.Tasks;
using GKCore;
using GKCore.Design;
using GKUI.Platform;
using GKUI.Themes;
using Microsoft.Maui.Controls;
using IView = GKCore.Design.IView;
using IViewController = GKCore.Design.IViewController;
using IWindow = GKCore.Design.IWindow;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public abstract class CommonForm : ContentPage, IView, IThemedView
    {
        public abstract IViewController Controller { get; }


        #region View Interface

        public bool Enabled
        {
            get { return base.IsEnabled; }
            set { base.IsEnabled = value; }
        }

        public bool Visible
        {
            get { return base.IsVisible; }
            set { base.IsVisible = value; }
        }

        #endregion

        public CommonForm()
        {
        }

        public void SetTitle(string value)
        {
            base.Title = value;
        }

        public void SetToolTip(object component, string toolTip)
        {
            // not supported
        }

        public void Activate()
        {
            Focus();
        }

        public object GetControl(string controlName)
        {
            return FindByName(controlName);
        }

        public virtual void Close()
        {
            Navigation.PopAsync();
        }

        public void Dispose()
        {
            // not supported
        }

        public virtual void ApplyTheme()
        {
            // not supported
        }

        public virtual bool SkipTheme(IDisposable component)
        {
            return false;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class CommonWindow<TView, TController> : CommonForm, IWindow
        where TView : IView
        where TController : FormController<TView>
    {
        protected TController fController;

        public override IViewController Controller { get { return fController; } }

        public virtual void Show(bool showInTaskbar)
        {
            // for this(BaseWinSDI) Navigation throws exception
            XFAppHost.GetMainPage().NavigationPage.PushAsync(this);
        }

        public virtual void SetLocale()
        {
        }
    }


    public enum DialogResult
    {
        None,
        Cancel,
        Ok
    }


    /// <summary>
    ///
    /// Ref: https://stackoverflow.com/questions/24174241/how-can-i-await-modal-form-dismissal-using-xamarin-forms
    /// </summary>
    public abstract class CommonDialog : CommonForm, ICommonDialog
    {
        private DialogResult fResult;
        private readonly TaskCompletionSource<bool> fTaskSource;

        public Task<bool> DialogResultTask
        {
            get { return fTaskSource.Task; }
        }

        public DialogResult DialogResult
        {
            get { return fResult; }
        }

        public CommonDialog()
        {
            fTaskSource = new TaskCompletionSource<bool>();
            fResult = DialogResult.None;
        }

        public override void Close()
        {
            Navigation.PopModalAsync();
        }

        protected async Task Close(DialogResult dialogResult)
        {
            fResult = dialogResult;
            if (fResult != DialogResult.None) {
                await Navigation.PopModalAsync();
                fTaskSource.SetResult(dialogResult == DialogResult.Ok);
            }
        }

        protected async virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            await Close(DialogResult.Ok);
        }

        protected async virtual void CancelClickHandler(object sender, EventArgs e)
        {
            await Close(DialogResult.Cancel);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class CommonDialog<TView, TController> : CommonDialog
        where TView : IView
        where TController : DialogController<TView>
    {
        protected TController fController;

        public override IViewController Controller { get { return fController; } }

        protected async override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                if (fController.Accept())
                    await Close(DialogResult.Ok);
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected async override void CancelClickHandler(object sender, EventArgs e)
        {
            try {
                if (await fController.CheckChangesPersistence())
                    return;

                if (await fController.Cancel())
                    await Close(DialogResult.Cancel);
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.CancelClickHandler()", ex);
            }
        }
    }
}
