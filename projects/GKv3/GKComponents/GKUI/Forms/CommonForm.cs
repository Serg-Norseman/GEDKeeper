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
using Eto.Forms;
using GKCore;
using GKCore.Design;
using GKCore.Interfaces;
using GKUI.Components;
using GKUI.Themes;

namespace GKUI.Forms
{
    /// <summary>
    /// 
    /// </summary>
    public class CommonForm : Form, IView, IThemedForm
    {
        private readonly ControlsManager fControlsManager;

        #region View Interface

        public new string Title
        {
            get { return base.Title; }
            set { base.Title = value; }
        }

        #endregion

        public CommonForm()
        {
            fControlsManager = new ControlsManager(this);
        }

        public void SetToolTip(object component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control) {
                    ((Control)component).ToolTip = toolTip;
                } else if (component is ToolItem) {
                    ((ToolItem)component).ToolTip = toolTip;
                }
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
            var field = this.GetType().GetField(controlName, BindingFlags.NonPublic | BindingFlags.Instance);
            object result = (field == null) ? null : field.GetValue(this);
            if (result == null) {
                result = this.FindChild(controlName);
            }
            return result;
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            ApplyTheme();
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
            AppHost.Instance.CloseWindow(this);
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
    public class CommonDialog : Dialog<DialogResult>, ICommonDialog, IThemedForm
    {
        private readonly ControlsManager fControlsManager;
        private readonly TaskCompletionSource<bool> fTaskSource;

        public Task<bool> DialogResultTask
        {
            get { return fTaskSource.Task; }
        }

        #region View Interface

        public new string Title
        {
            get { return base.Title; }
            set { base.Title = value; }
        }

        #endregion

        public DialogResult DialogResult
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
        }

        public CommonDialog()
        {
            Maximizable = false;
            Minimizable = false;
            Resizable = false;
            ShowInTaskbar = false;

            fControlsManager = new ControlsManager(this);

            fTaskSource = new TaskCompletionSource<bool>();

            // Option: Enter in dialogs is always the "Accept/Apply/Ok" button
            /*Styles.Add<Button>(null, b => {
                b.KeyDown += (sender, e) => {
                    var defButton = DefaultButton;
                    if (e.KeyData == Keys.Enter && sender != defButton && sender != AbortButton) {
                        defButton?.PerformClick();
                        e.Handled = true;
                    }
                };
            });*/
        }

        public void SetToolTip(object component, string toolTip)
        {
            if (component != null && !string.IsNullOrEmpty(toolTip)) {
                if (component is Control) {
                    ((Control)component).ToolTip = toolTip;
                } else if (component is ToolItem) {
                    ((ToolItem)component).ToolTip = toolTip;
                }
            }
        }

        public void Activate()
        {
            Focus();
        }

        protected new void Close(DialogResult dialogResult)
        {
            if (dialogResult != DialogResult.None) {
                fTaskSource.SetResult(dialogResult == DialogResult.Ok);
                DialogResult = dialogResult;
            }
        }

        protected virtual void AcceptClickHandler(object sender, EventArgs e)
        {
            Close(DialogResult.Ok);
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

        protected T GetControlHandler<T>(object control) where T : class, IControl
        {
            return fControlsManager.GetControl<T>(control);
        }

        public object GetControl(string controlName)
        {
            var field = this.GetType().GetField(controlName, BindingFlags.NonPublic | BindingFlags.Instance);
            object result = (field == null) ? null : field.GetValue(this);
            if (result == null) {
                result = this.FindChild(controlName);
            }
            return result;
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);
            ApplyTheme();
        }

        protected override void OnShown(EventArgs e)
        {
            base.OnShown(e);

            /*BringToFront();
            if (ParentWindow != null) {
                ParentWindow.SendToBack();
            }*/

            // Only for Windows7
            if (SysUtils.GetOSType() < OSType.Windows8 && Owner != null) {
                UIHelper.CenterFormByParent(this, Owner.Bounds);
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
    public class CommonDialog<TView, TController> : CommonDialog
        where TView : IView
        where TController : DialogController<TView>
    {
        protected TController fController;

        protected override void AcceptClickHandler(object sender, EventArgs e)
        {
            try {
                if (fController.Accept())
                    Close(DialogResult.Ok);
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
            var selectedTab = tabCtl.SelectedPage;
            if (selectedTab != null && selectedTab.Content != null) {
                selectedTab.Content.Focus();
            }
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();
            fController.ApplyTheme();
        }
    }
}
