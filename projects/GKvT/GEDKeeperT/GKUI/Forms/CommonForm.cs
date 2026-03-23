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
using Terminal.Gui;

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
            Application.RequestStop(this);
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public class CommonWindow : CommonForm, IWindow
    {
        public CommonWindow()
        {
            Loaded += Form_Load;
            Closed += Form_Closed;
        }

        public virtual void Show(bool showInTaskbar)
        {
            Application.Run(this);
        }

        public virtual void SetLocale()
        {
        }

        private void Form_Load(object sender, EventArgs e)
        {
            AppHost.Instance.LoadWindow(this);
        }

        private void Form_Closed(object sender, Toplevel top)
        {
            AppHost.Instance.CloseWindow(this);
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
            ButtonAlignment = Dialog.ButtonAlignments.Right;

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

        protected virtual void CancelClickHandler(object sender, EventArgs e)
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

        protected void Close(DialogResult dialogResult)
        {
            if (dialogResult != DialogResult.None) {
                //fTaskSource.SetResult(dialogResult == DialogResult.Ok);
                DialogResult = dialogResult;
            }
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

        protected virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                DialogResult = fController.Accept() ? DialogResult.Ok : DialogResult.None;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected override async void CancelClickHandler(object sender, EventArgs e)
        {
            try {
                if (await fController.Cancel())
                    DialogResult = DialogResult.Cancel;
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.CancelClickHandler()", ex);
            }
        }

        /*protected override void OnClosing(CancelEventArgs e)
        {
            base.OnClosing(e);
            e.Cancel = fController.CheckChangesPersistence();
        }*/

        /// <summary>
        /// This is necessary so that when user switch to the desired tab,
        /// its nested control can immediately receive events - this is useful for bulk data input.
        /// </summary>
        protected static void tabControl_SelectedTabChanged(object sender, EventArgs e)
        {
            /*var tabCtl = (TabControl)sender;
            var selectedTab = tabCtl.SelectedPage;
            if (selectedTab != null && selectedTab.Content != null) {
                selectedTab.Content.Focus();
            }*/
        }
    }
}
