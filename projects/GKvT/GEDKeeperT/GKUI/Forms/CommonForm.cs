/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GKCore;
using GKCore.Design;
using Terminal.Gui.App;
using Terminal.Gui.Input;
using Terminal.Gui.Views;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Window, IView
    {
        private readonly ControlsManager fControlsManager;

        #region View Interface

        public new string Title
        {
            get { return base.Title.ToString(); }
            set { base.Title = value; }
        }

        #endregion

        public CommonForm()
        {
            fControlsManager = new ControlsManager(this);
        }

        public void SetTitle(string title)
        {
            base.Title = title;
        }

        public void SetToolTip(object component, string toolTip)
        {
            // Not supported
        }

        public new void Activate()
        {
            SetFocus();
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControlHandler<T>(control);
        }

        public object GetControl(string controlName)
        {
            var field = this.GetType().GetField(controlName, BindingFlags.NonPublic | BindingFlags.Instance);
            object result = (field == null) ? null : field.GetValue(this);
            if (result == null) {
                // Not supported
            }
            return result;
        }

        public T GetCoreControl<T>(string controlName) where T : class, IControl
        {
            return fControlsManager.GetCoreControl<T>(controlName);
        }

        public void Close()
        {
            //throw new NotImplementedException();
        }

        protected override bool OnIsRunningChanging(bool oldIsRunning, bool newIsRunning)
        {
            if (!newIsRunning) {
                // Stopping
                return OnClosing();
            } else {
                OnLoading();
                return false;
            }
        }

        protected virtual void OnLoading()
        {
        }

        protected virtual bool OnClosing()
        {
            return false; // Cancel = false
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
            //Show();
        }

        public virtual void SetLocale()
        {
        }

        protected override void OnLoading()
        {
            AppHost.Instance.LoadWindow(this);
        }

        protected override bool OnClosing()
        {
            AppHost.Instance.CloseWindow(this);
            return false;
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonWindow<TView, TController> : CommonWindow
        where TView : IView
        where TController : FormController<TView>
    {
        protected TController fController;
    }


    public enum DialogResult
    {
        None,
        Ok,
        Cancel,
        Abort,
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonDialog : Dialog, ICommonDialog
    {
        private readonly ControlsManager fControlsManager;
        private DialogResult fResult;

        #region View Interface

        public new string Title
        {
            get { return base.Title.ToString(); }
            set { base.Title = value; }
        }

        #endregion

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
            DialogResult = DialogResult.None;

            fControlsManager = new ControlsManager(this);
        }

        public void SetTitle(string title)
        {
            base.Title = title;
        }

        public void SetToolTip(object component, string toolTip)
        {
            // Not supported
        }

        public new void Activate()
        {
            SetFocus();
        }

        public virtual bool ShowModalX(object owner)
        {
            //return (ShowModal((Control)owner) == DialogResult.Ok);
            return false;
        }

        protected virtual void CancelClickHandler(object sender, CommandEventArgs e)
        {
            DialogResult = DialogResult.Cancel;
            //Close(DialogResult.Cancel);
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControlHandler<T>(control);
        }

        public object GetControl(string controlName)
        {
            var field = this.GetType().GetField(controlName, BindingFlags.NonPublic | BindingFlags.Instance);
            object result = (field == null) ? null : field.GetValue(this);
            if (result == null) {
                // Not supported
            }
            return result;
        }

        public T GetCoreControl<T>(string controlName) where T : class, IControl
        {
            return fControlsManager.GetCoreControl<T>(controlName);
        }

        public void Close()
        {
            Application.RequestStop(this);
        }

        protected override bool OnIsRunningChanging(bool oldIsRunning, bool newIsRunning)
        {
            if (!newIsRunning) {
                // Stopping
                return OnClosing();
            } else {
                OnLoading();
                return false;
            }
        }

        protected virtual void OnLoading()
        {
        }

        protected virtual bool OnClosing()
        {
            return false; // Cancel = false
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

        protected virtual void AcceptClickHandler(object sender, CommandEventArgs e)
        {
            try {
                DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected override async void CancelClickHandler(object sender, CommandEventArgs e)
        {
            try {
                if (await fController.Cancel())
                    DialogResult = DialogResult.Cancel;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.CancelClickHandler()", ex);
            }
        }

        protected override bool OnClosing()
        {
            // FIXME
            bool cancel = false; // fController.CheckChangesPersistence();
            return cancel;
        }
    }
}
