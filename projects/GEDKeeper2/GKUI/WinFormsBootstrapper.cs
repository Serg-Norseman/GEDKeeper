/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using System.Windows.Forms;

using GKCommon;
using GKCommon.IoC;
using GKCore;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.UIContracts;
using GKUI.Charts;
using GKUI.Components;
using GKUI.Dialogs;

namespace GKUI
{
    public class ToolStripMenuItemEx : ToolStripMenuItem, IMenuItem
    {
        public ToolStripMenuItemEx(string text) : base(text)
        {
        }
    }

    public sealed class WinFormsAppHost : AppHost
    {
        private readonly ApplicationContext fAppContext;

        public ApplicationContext AppContext
        {
            get { return fAppContext; }
        }

        public WinFormsAppHost() : base()
        {
            fAppContext = new ApplicationContext();
            Application.ApplicationExit += new EventHandler(this.OnApplicationExit);
        }

        public override void Init(string[] args, bool isMDI)
        {
            base.Init(args, isMDI);

            if (fIsMDI) {
                fAppContext.MainForm = (Form)fMainWindow;
            }
        }

        public override IntPtr GetTopWindowHandle()
        {
            IntPtr mainHandle = IntPtr.Zero;
            if (fIsMDI && fMainWindow != null) {
                mainHandle = ((Form)fMainWindow).Handle;
            }
            return mainHandle;
        }

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (!fIsMDI && fRunningForms.Count == 0) {
                fAppContext.ExitThread();
            }
        }

        private void OnApplicationExit(object sender, EventArgs e)
        {
        }

        public override IWindow GetActiveMdiChild()
        {
            if (fIsMDI) {
                return (IWindow)((Form)fMainWindow).ActiveMdiChild;
            } else {
                return null;
            }
        }

        public override void ShowWindow(IWindow window, bool taskbar)
        {
            Form frm = window as Form;

            if (frm != null) {
                frm.ShowInTaskbar = taskbar;
                if (fIsMDI) {
                    frm.MdiParent = (Form)fMainWindow;
                }
                frm.Show();
            }
        }

        public override bool ShowModalX(ICommonDialog form, bool keepModeless)
        {
            IntPtr mainHandle = ((Form)fMainWindow).Handle;

            if (keepModeless) {
                #if !__MonoCS__
                NativeMethods.PostMessage(mainHandle, NativeMethods.WM_KEEPMODELESS, IntPtr.Zero, IntPtr.Zero);
                #endif
            }

            UIHelper.CenterFormByParent((Form)form, mainHandle);

            return base.ShowModalX(form, keepModeless);
        }

        public override void EnableWindow(IWidgetForm form, bool value)
        {
            Form frm = form as Form;

            if (frm != null) {
                #if !__MonoCS__
                NativeMethods.EnableWindow(frm.Handle, value);
                #endif
            }
        }

        protected override void UpdateLang()
        {
            if (fIsMDI && fMainWindow != null) {
                var mdiForm = fMainWindow as Form;
                foreach (Form child in mdiForm.MdiChildren) {
                    ILocalization localChild = (child as ILocalization);

                    if (localChild != null) {
                        localChild.SetLang();
                    }
                }

                fMainWindow.SetLang();
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            base.BaseClosed(baseWin);

            if (fIsMDI && fMainWindow != null) {
                ((MainWin)fMainWindow).CheckMRUWin(baseWin.Context.FileName, (Form)baseWin);
            }
        }

        protected override void UpdateMRU()
        {
            if (fIsMDI && fMainWindow != null) {
                ((MainWin)fMainWindow).UpdateMRU();
            }
        }
    }

    /// <summary>
    /// This class implements initialization of IoC-container for WinForms presentation.
    /// </summary>
    public static class WinFormsBootstrapper
    {
        public static void Configure(IContainer container)
        {
            if (container == null)
                throw new ArgumentNullException("container");

            container.Register<IStdDialogs, WinFormsStdDialogs>(LifeCycle.Singleton);
            container.Register<IUtilities, Utilities>(LifeCycle.Singleton);
            //container.Register<ILogger, LoggerStub>(LifeCycle.Singleton);
            container.Register<IProgressController, ProgressController>(LifeCycle.Singleton);

            // controls and other
            container.Register<ITreeChartBox, TreeChartBox>(LifeCycle.Transient);
            //container.Register<IWizardPages, WizardPages>(LifeCycle.Transient);

            // dialogs
            container.Register<IRecordSelectDialog, RecordSelectDlg>(LifeCycle.Transient);
            container.Register<IAddressEditDlg, AddressEditDlg>(LifeCycle.Transient);
            container.Register<IAssociationEditDlg, AssociationEditDlg>(LifeCycle.Transient);
            container.Register<ICommunicationEditDlg, CommunicationEditDlg>(LifeCycle.Transient);
            container.Register<IEventEditDlg, EventEditDlg>(LifeCycle.Transient);
            container.Register<IFamilyEditDlg, FamilyEditDlg>(LifeCycle.Transient);
            container.Register<IGroupEditDlg, GroupEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageEditDlg, LanguageEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageSelectDlg, LanguageSelectDlg>(LifeCycle.Transient);
            container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlgEx, NoteEditDlgEx>(LifeCycle.Transient);
            container.Register<IPersonalNameEditDlg, PersonalNameEditDlg>(LifeCycle.Transient);
            container.Register<IPersonEditDlg, PersonEditDlg>(LifeCycle.Transient);
            container.Register<IRepositoryEditDlg, RepositoryEditDlg>(LifeCycle.Transient);
            container.Register<IResearchEditDlg, ResearchEditDlg>(LifeCycle.Transient);
            container.Register<ISexCheckDlg, SexCheckDlg>(LifeCycle.Transient);
            container.Register<ISourceCitEditDlg, SourceCitEditDlg>(LifeCycle.Transient);
            container.Register<ISourceEditDlg, SourceEditDlg>(LifeCycle.Transient);
            container.Register<ITaskEditDlg, TaskEditDlg>(LifeCycle.Transient);
            container.Register<IUserRefEditDlg, UserRefEditDlg>(LifeCycle.Transient);
            container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            container.Register<IPortraitSelectDlg, PortraitSelectDlg>(LifeCycle.Transient);

            container.Register<IBaseWindow, BaseWin>(LifeCycle.Transient);
            container.Register<IMainWindow, MainWin>(LifeCycle.Transient);
            container.Register<IDayTipsDlg, DayTipsDlg>(LifeCycle.Transient);
        }
    }
}
