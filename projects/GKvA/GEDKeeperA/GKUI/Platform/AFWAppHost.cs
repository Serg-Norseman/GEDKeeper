﻿/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2021 by Sergey V. Zhdanovskih.
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
using Avalonia.Controls;
using BSLib.Design.Graphics;
using BSLib.Design.IoC;
using BSLib.Design.MVP;
using GKCore;
using GKCore.Interfaces;
using GKCore.MVP.Views;
using GKUI.Components;
using GKUI.Forms;

namespace GKUI.Platform
{
    public class AFWAppHost
    {
        #region Bootstrapper

        /// <summary>
        /// This function implements initialization of IoC-container for UWP presentation.
        /// </summary>
        public static void ConfigureBootstrap(bool mdi)
        {
            //var appHost = new EtoAppHost();

            IContainer container = AppHost.Container;

            if (container == null)
                throw new ArgumentNullException("container");

            container.Reset();

            // controls and other
            //container.Register<IStdDialogs, EtoStdDialogs>(LifeCycle.Singleton);
            container.Register<IGraphicsProviderEx, AFWGfxProvider>(LifeCycle.Singleton);
            //container.Register<IProgressController, ProgressController>(LifeCycle.Singleton);
            //container.Register<ITreeChartBox, TreeChartBox>(LifeCycle.Transient);
            // dialogs
            container.Register<IAboutDlg, AboutDlg>(LifeCycle.Transient);
            /*
            container.Register<IAddressEditDlg, AddressEditDlg>(LifeCycle.Transient);
            container.Register<IAssociationEditDlg, AssociationEditDlg>(LifeCycle.Transient);
            container.Register<IBaseWindow, BaseWinSDI>(LifeCycle.Transient);
            container.Register<ICircleChartWin, CircleChartWin>(LifeCycle.Transient);
            container.Register<ICommunicationEditDlg, CommunicationEditDlg>(LifeCycle.Transient);
            container.Register<ICommonFilterDlg, CommonFilterDlg>(LifeCycle.Transient);
            container.Register<IDayTipsDlg, DayTipsDlg>(LifeCycle.Transient);
            container.Register<IEventEditDlg, EventEditDlg>(LifeCycle.Transient);
            container.Register<IFamilyEditDlg, FamilyEditDlg>(LifeCycle.Transient);
            container.Register<IFilePropertiesDlg, FilePropertiesDlg>(LifeCycle.Transient);
            container.Register<IFragmentSearchDlg, TTFamilyGroupsDlg>(LifeCycle.Transient);
            container.Register<IGroupEditDlg, GroupEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageEditDlg, LanguageEditDlg>(LifeCycle.Transient);
            container.Register<ILanguageSelectDlg, LanguageSelectDlg>(LifeCycle.Transient);
            container.Register<ILocationEditDlg, LocationEditDlg>(LifeCycle.Transient);
            container.Register<IMapsViewerWin, MapsViewerWin>(LifeCycle.Transient);
            container.Register<IMediaEditDlg, MediaEditDlg>(LifeCycle.Transient);
            container.Register<INameEditDlg, NameEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlg, NoteEditDlg>(LifeCycle.Transient);
            container.Register<INoteEditDlgEx, NoteEditDlgEx>(LifeCycle.Transient);
            container.Register<IOptionsDlg, OptionsDlg>(LifeCycle.Transient);
            container.Register<IOrganizerWin, OrganizerWin>(LifeCycle.Transient);
            container.Register<IPatriarchsSearchDlg, TTPatSearchDlg>(LifeCycle.Transient);
            */
            container.Register<IPatriarchsViewer, PatriarchsViewerWin>(LifeCycle.Transient);
            /*
            container.Register<IPersonsFilterDlg, PersonsFilterDlg>(LifeCycle.Transient);
            container.Register<IPlacesManagerDlg, TTPlacesManagerDlg>(LifeCycle.Transient);
            container.Register<IPersonalNameEditDlg, PersonalNameEditDlg>(LifeCycle.Transient);
            container.Register<IPersonEditDlg, PersonEditDlg>(LifeCycle.Transient);
            container.Register<IPortraitSelectDlg, PortraitSelectDlg>(LifeCycle.Transient);
            container.Register<IRecMergeDlg, TTRecMergeDlg>(LifeCycle.Transient);
            container.Register<IRecordSelectDialog, RecordSelectDlg>(LifeCycle.Transient);
            container.Register<IRelationshipCalculatorDlg, RelationshipCalculatorDlg>(LifeCycle.Transient);
            container.Register<IRepositoryEditDlg, RepositoryEditDlg>(LifeCycle.Transient);
            container.Register<IResearchEditDlg, ResearchEditDlg>(LifeCycle.Transient);
            container.Register<ISexCheckDlg, SexCheckDlg>(LifeCycle.Transient);
            container.Register<ISourceCitEditDlg, SourceCitEditDlg>(LifeCycle.Transient);
            container.Register<ISourceEditDlg, SourceEditDlg>(LifeCycle.Transient);
            container.Register<IScriptEditWin, ScriptEditWin>(LifeCycle.Transient);
            container.Register<ISlideshowWin, SlideshowWin>(LifeCycle.Transient);
            container.Register<IStatisticsWin, StatisticsWin>(LifeCycle.Transient);
            container.Register<ITaskEditDlg, TaskEditDlg>(LifeCycle.Transient);
            container.Register<ITreeChartWin, TreeChartWin>(LifeCycle.Transient);
            container.Register<ITreeCheckDlg, TTTreeCheckDlg>(LifeCycle.Transient);
            container.Register<ITreeCompareDlg, TTTreeCompareDlg>(LifeCycle.Transient);
            container.Register<ITreeFilterDlg, TreeFilterDlg>(LifeCycle.Transient);
            container.Register<ITreeMergeDlg, TTTreeMergeDlg>(LifeCycle.Transient);
            container.Register<ITreeSplitDlg, TTTreeSplitDlg>(LifeCycle.Transient);
            */
            container.Register<IUserRefEditDlg, UserRefEditDlg>(LifeCycle.Transient);

            ControlsManager.RegisterHandlerType(typeof(Button), typeof(ButtonHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(CheckBox), typeof(CheckBoxHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(AutoCompleteBox), typeof(ComboBoxHandler)); // AvUI: (-) ComboBox isn't editable
            ControlsManager.RegisterHandlerType(typeof(TextBlock), typeof(LabelHandler)); // AvUI: +
            //ControlsManager.RegisterHandlerType(typeof(MaskedTextBox), typeof(MaskedTextBoxHandler)); // AvUI: (-)
            ControlsManager.RegisterHandlerType(typeof(NumericUpDown), typeof(NumericBoxHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(ProgressBar), typeof(ProgressBarHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(RadioButton), typeof(RadioButtonHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(TabControl), typeof(TabControlHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(TextBox), typeof(TextBoxHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(TreeView), typeof(TreeViewHandler)); // AvUI: +
            ControlsManager.RegisterHandlerType(typeof(MenuItem), typeof(MenuItemHandler)); // AvUI: + MenuItem

            //ControlsManager.RegisterHandlerType(typeof(LogChart), typeof(LogChartHandler)); // custom
        }

        #endregion
    }
}
