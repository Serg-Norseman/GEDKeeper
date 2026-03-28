/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Globalization;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using GKCore;
using GKCore.Charts;
using GKCore.Design;
using GKCore.Design.Graphics;
using GKCore.Design.Views;
using GKCore.Utilities;
using GKCore.Validation;
using GKUI.Components;
using GKUI.Forms;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Platform
{
    /// <summary>
    /// The main implementation of the platform-specific application's host for
    /// Terminal.Gui.
    /// </summary>
    public sealed class TGAppHost : AppHost
    {
        static TGAppHost()
        {
            SetAppSign("GEDKeeperR");
        }

        public TGAppHost()
        {
        }

        public override void CloseWindow(IWindow window)
        {
            base.CloseWindow(window);

            if (fRunningForms.Count == 0) {
                Quit();
            }
        }

        public override IForm GetActiveForm()
        {
            throw new NotImplementedException();
        }

        public override IWindow GetActiveWindow()
        {
            return (fRunningForms != null && fRunningForms.Count > 0) ? fRunningForms[0] : null;
        }

        public override async Task<bool> ShowModalAsync(ICommonDialog dialog, IView owner, bool keepModeless = false)
        {
            try {
                var tgDlg = dialog as CommonDialog;
                Application.Run(tgDlg);
                return (tgDlg.DialogResult == DialogResult.Ok);
            } catch (Exception ex) {
                Logger.WriteError("TGAppHost.ShowModalAsync()", ex);
                return false;
            }
        }

        public override GKCore.ITimer CreateTimer(double msInterval, EventHandler elapsedHandler)
        {
            return new TGTimer(msInterval, elapsedHandler);
        }

        public override void Quit()
        {
            Application.RequestStop();
        }

        public override bool ExecuteWork(ProgressStart proc, string title = "")
        {
            using (var progressForm = ResolveDialog<IProgressDialog>()) {
                var progForm = progressForm as CommonDialog;

                if (!string.IsNullOrEmpty(title))
                    progressForm.SetTitle(title);

                var workerThread = new Thread((obj) => {
                    proc((IProgressController)obj);
                });

                Application.RunState rs;
                DialogResult dialogResult = DialogResult.Abort;
                try {
                    workerThread.Start(progressForm);

                    ///*dialogResult =*/ Application.Run(progForm);

                    rs = Application.Begin(progForm);

                    Application.MainLoop.AddTimeout(TimeSpan.FromMilliseconds(10), x => true);
                } finally {
                    workerThread.Join();
                }
                Application.End(rs);

                return true;
            }
        }

        public override bool HasFeatureSupport(Feature feature)
        {
            bool result = false;
            switch (feature) {
                case Feature.RecentFilesLoad:
                    result = true;
                    break;

                default:
                    result = false;
                    break;
            }
            return result;
        }

        public override void Invoke(Action action)
        {
            action();
        }

        public override int GetKeyLayout()
        {
            // InputLanguage only exists in WinForms
            return CultureInfo.CurrentUICulture.KeyboardLayoutId;
        }

        public override void SetKeyLayout(int layout)
        {
            try {
                CultureInfo.DefaultThreadCurrentUICulture = new CultureInfo(layout);
            } catch (Exception ex) {
                Logger.WriteError("TGAppHost.SetKeyLayout()", ex);
            }
        }

        public override void SetClipboardText(string text)
        {
            Application.Driver.Clipboard.SetClipboardData(text);
        }

        /// <summary>
        /// This function implements initialization of IoC-container for WinForms presentation.
        /// </summary>
        public static void ConfigureBootstrap()
        {
#if NETCOREAPP3_1_OR_GREATER
            // support for legacy encodings
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);
#endif

            var appHost = new TGAppHost();

            var container = AppHost.Container;
            container.Reset();
            ValidationFactory.InitGDMValidators();

            // controls and other
            container.Register<IStdDialogs, TGStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProvider, TGGfxProvider>(LifeCycle.Singleton);
            container.Register<ITreeChart, TreeChartBox>(LifeCycle.Transient);

            // dialogs
            container.Register<IAboutDlg, AboutDlg>(LifeCycle.Transient);
            container.Register<IAddressEditDlg, AddressEditDlg>(LifeCycle.Transient);
            container.Register<IAssociationEditDlg, AssociationEditDlg>(LifeCycle.Transient);
            //container.Register<IBaseWindow, BaseWinSDI>(LifeCycle.Transient); // breaks menu and statusbar
            container.Register<ICommunicationEditDlg, CommunicationEditDlg>(LifeCycle.Transient);
            container.Register<ICommonFilterDlg, CommonFilterDlg>(LifeCycle.Transient);
            container.Register<IEventEditDlg, EventEditDlg>(LifeCycle.Transient);
            container.Register<IFamilyEditDlg, FamilyEditDlg>(LifeCycle.Transient);
            container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            container.Register<IFragmentSearchDlg, TTFamilyGroupsDlg>(LifeCycle.Transient);
            container.Register<IGroupEditDlg, GroupEditDlg>(LifeCycle.Transient);
            container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            container.Register<ILocationNameEditDlg, LocationNameEditDlg>(LifeCycle.Transient);
            container.Register<ILocationLinkEditDlg, LocationLinkEditDlg>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            container.Register<IMediaFileEditDlg, MediaFileEditDlg>(LifeCycle.Transient);
            container.Register<IMediaViewerWin, MediaViewerWin>(LifeCycle.Transient);
            container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            container.Register<IOptionsDlg, OptionsDlg>(LifeCycle.Transient);
            container.Register<IOrganizerWin, OrganizerWin>(LifeCycle.Transient);
            container.Register<IParentsEditDlg, ParentsEditDlg>(LifeCycle.Transient);
            container.Register<IPatriarchsSearchDlg, TTPatSearchDlg>(LifeCycle.Transient);
            container.Register<IPersonsFilterDlg, PersonsFilterDlg>(LifeCycle.Transient);
            container.Register<IPlacesManagerDlg, TTPlacesManagerDlg>(LifeCycle.Transient);
            container.Register<IPersonalNameEditDlg, PersonalNameEditDlg>(LifeCycle.Transient);
            container.Register<IPersonEditDlg, PersonEditDlg>(LifeCycle.Transient);
            container.Register<IRecMergeDlg, TTRecMergeDlg>(LifeCycle.Transient);
            container.Register<IRecordSelectDialog, RecordSelectDlg>(LifeCycle.Transient);
            container.Register<IRelationshipCalculatorDlg, RelationshipCalculatorDlg>(LifeCycle.Transient);
            container.Register<IRepositoryEditDlg, RepositoryEditDlg>(LifeCycle.Transient);
            container.Register<IResearchEditDlg, ResearchEditDlg>(LifeCycle.Transient);
            container.Register<ISexCheckDlg, SexCheckDlg>(LifeCycle.Transient);
            container.Register<ISourceCitEditDlg, SourceCitEditDlg>(LifeCycle.Transient);
            container.Register<ISourceEditDlg, SourceEditDlg>(LifeCycle.Transient);
            //container.Register<IScriptEditWin, ScriptEditWin>(LifeCycle.Transient);
            container.Register<IStatisticsWin, StatisticsWin>(LifeCycle.Transient);
            container.Register<ITaskEditDlg, TaskEditDlg>(LifeCycle.Transient);
            container.Register<ITreeChartWin, TreeChartWin>(LifeCycle.Transient);
            container.Register<ITreeCheckDlg, TTTreeCheckDlg>(LifeCycle.Transient);
            container.Register<ITreeCompareDlg, TTTreeCompareDlg>(LifeCycle.Transient);
            container.Register<ITreeFilterDlg, TreeFilterDlg>(LifeCycle.Transient);
            container.Register<ITreeMergeDlg, TTTreeMergeDlg>(LifeCycle.Transient);
            container.Register<ITreeSplitDlg, TTTreeSplitDlg>(LifeCycle.Transient);
            container.Register<IUserRefEditDlg, UserRefEditDlg>(LifeCycle.Transient);
            container.Register<IRecordInfoDlg, RecordInfoDlg>(LifeCycle.Transient);
            container.Register<IFARDlg, FindAndReplaceDlg>(LifeCycle.Transient);
            container.Register<IEventDefEditDlg, EventDefEditDlg>(LifeCycle.Transient);
            container.Register<ISourceCallNumberEditDlg, SourceCallNumberEditDlg>(LifeCycle.Transient);
            container.Register<IRepositoryCitEditDlg, RepositoryCitEditDlg>(LifeCycle.Transient);
            container.Register<IDNATestEditDlg, DNATestEditDlg>(LifeCycle.Transient);
            container.Register<IAgeEditDlg, AgeEditDlg>(LifeCycle.Transient);
            container.Register<IChronicleWin, ChronicleWin>(LifeCycle.Transient);
            container.Register<IProgressDialog, ProgressDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ComboBox), typeof(ComboBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(FrameView), typeof(GroupBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(Label), typeof(LabelHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(MenuBarItem), typeof(MenuItemHandler));
            ControlsManager.RegisterHandlerType(typeof(NumericStepper), typeof(NumericBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler));
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler));
            ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler));
            ControlsManager.RegisterHandlerType(typeof(TabPage), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(TabView), typeof(TabControlHandler));
            ControlsManager.RegisterHandlerType(typeof(TabPage), typeof(TabPageHandler));
            ControlsManager.RegisterHandlerType(typeof(TextField), typeof(TextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TextValidateField), typeof(MaskedTextBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(TextView), typeof(TextAreaHandler));

            ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateBox), typeof(DateBoxHandler));
            ControlsManager.RegisterHandlerType(typeof(GKDateControl), typeof(DateControlHandler));
            ControlsManager.RegisterHandlerType(typeof(GKListView), typeof(ListViewHandler));
        }

        public static void Startup(string[] args)
        {
            ConfigureBootstrap();
            StartupCore(args);
        }
    }
}
