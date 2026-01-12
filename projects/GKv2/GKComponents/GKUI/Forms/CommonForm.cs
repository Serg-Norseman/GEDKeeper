/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.ComponentModel;
using System.Reflection;
using System.Threading.Tasks;
using System.Windows.Forms;
using GKCore;
using GKCore.Design;
using GKUI.Themes;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Form, IView, IThemedForm
    {
        private readonly IContainer fComponents;
        private readonly ToolTip fToolTip;

        private readonly ControlsManager fControlsManager;

        public CommonForm()
        {
            fComponents = new Container();
            fToolTip = new ToolTip(this.fComponents);

            fControlsManager = new ControlsManager(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fControlsManager.Clear();
                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetTitle(string value)
        {
            base.Text = value;
        }

        public void SetToolTip(object component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control control) {
                    fToolTip.SetToolTip(control, toolTip);
                } else if (component is ToolStripItem toolItem) {
                    toolItem.ToolTipText = toolTip;
                }
            }
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
                Control[] controls = this.Controls.Find(controlName, true);
                result = (controls != null && controls.Length > 0) ? controls[0] : null;
            }
            return result;
        }

        public T GetCoreControl<T>(string controlName) where T : class, IControl
        {
            return fControlsManager.GetCoreControl<T>(controlName);
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            if (!DesignMode) {
                ApplyTheme();
            }
        }

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
            ShowInTaskbar = showInTaskbar;
            Show();
        }

        public virtual void SetLocale()
        {
        }

        protected override void OnLoad(EventArgs e)
        {
            AppHost.Instance.LoadWindow(this);
            base.OnLoad(e);
        }

        protected override void OnClosed(EventArgs e)
        {
            if (!DesignMode) {
                AppHost.Instance.CloseWindow(this);
            }
            base.OnClosed(e);
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


        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fController != null) fController.Dispose();
            }
            base.Dispose(disposing);
        }

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
        private readonly TaskCompletionSource<bool> fTaskSource;

        public Task<bool> DialogResultTask
        {
            get { return fTaskSource.Task; }
        }

        public CommonDialog()
        {
            fTaskSource = new TaskCompletionSource<bool>();
        }

        protected void Close(DialogResult dialogResult)
        {
            if (dialogResult != DialogResult.None) {
                base.DialogResult = dialogResult;
                fTaskSource.SetResult(dialogResult == DialogResult.OK);
            }
        }

        protected virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            Close(DialogResult.OK);
        }

        protected virtual void CancelClickHandler(object sender, EventArgs e)
        {
            Close(DialogResult.Cancel);
        }

        protected override void OnClosing(CancelEventArgs e)
        {
            // To correctly close the dialog using the button in the dialog header.
            if (!e.Cancel && !fTaskSource.Task.IsCompleted)
                fTaskSource.SetResult(false);

            base.OnClosing(e);
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

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fController != null) fController.Dispose();
            }
            base.Dispose(disposing);
        }

        protected override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                if (fController.Accept())
                    Close(DialogResult.OK);
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.AcceptClickHandler()", ex);
            }
        }

        protected override async void CancelClickHandler(object sender, EventArgs e)
        {
            try {
                if (await fController.Cancel())
                    Close(DialogResult.Cancel);
            } catch (Exception ex) {
                Logger.WriteError("CommonDialog<>.CancelClickHandler()", ex);
            }
        }

        protected override async void OnClosing(CancelEventArgs e)
        {
            e.Cancel = await fController.CheckChangesPersistence();
            base.OnClosing(e);
        }

        /// <summary>
        /// This is necessary so that when user switch to the desired tab,
        /// its nested control can immediately receive events - this is useful for bulk data input.
        /// </summary>
        protected static void tabControl_SelectedIndexChanged(object sender, EventArgs e)
        {
            var tabCtl = (TabControl)sender;
            var selectedTab = tabCtl.SelectedTab;
            if (selectedTab != null && selectedTab.Controls.Count >= 1) {
                selectedTab.Controls[0].Focus();
            }
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
