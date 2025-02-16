/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.ComponentModel;
using System.Reflection;
using System.Threading.Tasks;
using System.Windows.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Interfaces;
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

        #region View Interface

        public string Title
        {
            get { return base.Text; }
            set { base.Text = value; }
        }

        #endregion

        public CommonForm()
        {
            fComponents = new Container();
            fToolTip = new ToolTip(this.fComponents);

            fControlsManager = new ControlsManager(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                if (fComponents != null) fComponents.Dispose();
            }
            base.Dispose(disposing);
        }

        public void SetToolTip(object component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control) {
                    fToolTip.SetToolTip((Control)component, toolTip);
                } else if (component is ToolStripItem) {
                    ((ToolStripItem)component).ToolTipText = toolTip;
                }
            }
        }

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControl<T>(control);
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
            if (!DesignMode) {
                AppHost.Instance.LoadWindow(this);
            }
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
