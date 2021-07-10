/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2020 by Sergey V. Zhdanovskih.
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

using System.IO;
using BSLib;
using BSLib.Design.Graphics;
using BSLib.Design.MVP;
using BSLib.Design.MVP.Controls;
using GDModel;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.Names;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.MVP.Views
{
    public interface IAboutDlg : ICommonDialog
    {
    }


    public interface IAddressEditDlg : ICommonDialog, IBaseEditor
    {
        GDMAddress Address { get; set; }

        ITextBox Country { get; }
        ITextBox State { get; }
        ITextBox City { get; }
        ITextBox PostalCode { get; }
        ITextBox AddressLine { get; }

        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IAssociationEditDlg : ICommonDialog, IBaseEditor
    {
        GDMAssociation Association { get; set; }

        ITextBox Person { get; }
        IComboBox Relation { get; }
    }


    public interface IBaseWindowView : IBaseWindow
    {
        ITabControl RecordTabs { get; }
        IMenuItem ReportsItem { get; }
        IMenuItem PluginsItem { get; }

        bool CheckModified();
    }


    public interface ICircleChartWin : IChartWindow
    {
        ICircleChart CircleChart { get; }
    }


    public interface ICommonFilterDlg : ICommonDialog
    {
        IFilterGridView FilterGrid { get; }
    }


    public interface ICommunicationEditDlg : ICommonDialog, IBaseEditor
    {
        GDMCommunicationRecord Communication { get; set; }

        ITextBox Corresponder { get; }
        IComboBox CorrType { get; }
        IDateBox Date { get; }
        IComboBox Dir { get; }
        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
    }


    public interface IDayTipsDlg : ICommonDialog
    {
        bool ShowTipsChecked { get; set; }

        void Init(string caption, bool showTipsChecked, StringList tips);

        ILabel TitleLabel { get; }
        ITextBox TipText { get; }
        IButton NextButton { get; }
    }


    public interface IEventEditDlg : ICommonDialog, IBaseEditor
    {
        GDMCustomEvent Event { get; set; }

        IComboBox EventType { get; }
        IComboBox EventDateType { get; }

        ICheckBox Date1BC { get; }
        ICheckBox Date2BC { get; }

        IComboBox Date1Calendar { get; }
        IComboBox Date2Calendar { get; }

        IDateBox Date1 { get; }
        IDateBox Date2 { get; }

        IComboBox Attribute { get; }
        ITextBox Place { get; }
        ITextBox EventName { get; }
        ITextBox Cause { get; }
        ITextBox Agency { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }

        void SetLocationMode(bool active);
    }


    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor
    {
        GDMFamilyRecord Family { get; set; }

        void SetTarget(TargetMode targetType, GDMIndividualRecord target);
        void LockEditor(bool locked);
        void SetHusband(string value);
        void SetWife(string value);

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ChildrenList { get; }
        ISheetList EventsList { get; }

        IComboBox MarriageStatus { get; }
        IComboBox Restriction { get; }
        ITextBox Husband { get; }
        ITextBox Wife { get; }
    }


    public interface IFilePropertiesDlg : ICommonDialog, IBaseEditor
    {
        IListView RecordStats { get; }

        ITextBox Language { get; }
        ITextBox Name { get; }
        ITextBox Address { get; }
        ITextBox Tel { get; }
    }


    public interface IGroupEditDlg : ICommonDialog, IBaseEditor
    {
        GDMGroupRecord Group { get; set; }

        ITextBox Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList MembersList { get; }
    }


    public interface ILanguageEditDlg : ICommonDialog
    {
        GDMLanguageID LanguageID { get; set; }

        IComboBox LanguageCombo { get; }
    }


    public interface ILanguageSelectDlg : ICommonDialog
    {
        int SelectedLanguage { get; set; }

        IListViewEx LanguagesList { get; }
    }


    public interface ILocationEditDlg : ICommonDialog, IBaseEditor
    {
        GDMLocationRecord LocationRecord { get; set; }

        IMapBrowser MapBrowser { get; }
        ISheetList MediaList { get; }
        ISheetList NotesList { get; }
        IListView GeoCoordsList { get; }
        ITextBox Name { get; }
        ITextBox Latitude { get; }
        ITextBox Longitude { get; }
    }


    public interface IMapsViewerWin : IWindow
    {
        IMapBrowser MapBrowser { get; }
        IComboBox PersonsCombo { get; }
        ITreeView PlacesTree { get; }
        IButton SelectPlacesBtn { get; }
        ICheckBox BirthCheck { get; }
        ICheckBox DeathCheck { get; }
        ICheckBox ResidenceCheck { get; }
        ICheckBox LinesVisibleCheck { get; }
        IRadioButton TotalRadio { get; }
        IRadioButton SelectedRadio { get; }

        ITVNode FindTreeNode(string place);
    }


    public interface IMediaEditDlg : ICommonDialog, IBaseEditor
    {
        GDMMultimediaRecord MediaRec { get; set; }

        ISheetList NotesList { get; }
        ISheetList SourcesList { get; }

        IComboBox MediaType { get; }
        IComboBox StoreType { get; }
        ITextBox Name { get; }
        ITextBox File { get; }
        IButton FileSelectButton { get; }
    }


    public interface IMediaViewerWin : IWindow
    {
        GDMFileReferenceWithTitle FileRef { get; set; }
        GDMMultimediaRecord Multimedia { get; set; }

        void SetViewImage(IImage img, GDMFileReferenceWithTitle fileRef);
        void SetViewMedia(string mediaFile);
        void SetViewText(string text);
        void SetViewRTF(string text);
        void SetViewHTML(Stream stm);
        void DisposeViewControl();
    }


    public interface INameEditDlg : ICommonDialog
    {
        NameEntry IName { get; set; }

        ITextBox Name { get; }
        ITextBox FPatr { get; }
        ITextBox MPatr { get; }
        IComboBox SexCombo { get; }
    }


    public interface INoteEdit : ICommonDialog, IBaseEditor
    {
        GDMNoteRecord NoteRecord { get; set; }

        ITextBox Note { get; }
    }


    public interface INoteEditDlg : INoteEdit
    {
    }

    public interface INoteEditDlgEx : INoteEdit
    {
    }


    public interface IOrganizerWin : ICommonDialog
    {
        ISheetList AdrList { get; }
        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IOptionsDlg : ICommonDialog
    {
        void SetPage(OptionsPage page);
    }


    public interface IParentsEditDlg : ICommonDialog, IBaseEditor
    {
        GDMChildToFamilyLink Link { get; set; }
        GDMIndividualRecord Person { get; set; }

        ITextBox Father { get; }
        ITextBox Mother { get; }
        ITextBox ChildName { get; }
        IComboBox LinkageTypeCombo { get; }

        void SetParentsAvl(bool avail);
        void SetFatherAvl(bool avail);
        void SetMotherAvl(bool avail);
    }


    public interface IPersonalNameEditDlg : ICommonDialog, IBaseEditor
    {
        GDMIndividualRecord Individual { get; set; }
        GDMPersonalName PersonalName { get; set; }

        ILabel SurnameLabel { get; }
        ITextBox Surname { get; }
        ITextBox Name { get; }
        ITextBox Patronymic { get; }
        IComboBox NameType { get; }
        ITextBox NamePrefix { get; }
        ITextBox Nickname { get; }
        ITextBox SurnamePrefix { get; }
        ITextBox NameSuffix { get; }
        ITextBox MarriedSurname { get; }
        IComboBox Language { get; }
    }


    public interface IPersonEditDlg : ICommonDialog, IBaseEditor
    {
        GDMIndividualRecord Person { get; set; }
        GDMIndividualRecord Target { get; set; }
        TargetMode TargetMode { get; set; }
        void SetNeedSex(GDMSex needSex);

        ISheetList EventsList { get; }
        ISheetList SpousesList { get; }
        ISheetList AssociationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList UserRefList { get; }
        ISheetList NamesList { get; }
        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ParentsList { get; }

        IPortraitControl Portrait { get; }
        ITextBox Father { get; }
        ITextBox Mother { get; }
        ITextBox Surname { get; }
        ITextBox Name { get; }
        IComboBox Patronymic { get; }
        ITextBox NamePrefix { get; }
        ITextBox Nickname { get; }
        ITextBox SurnamePrefix { get; }
        ITextBox NameSuffix { get; }
        ITextBox MarriedSurname { get; }

        ILabel SurnameLabel { get; }
        IComboBox RestrictionCombo { get; }
        IComboBox SexCombo { get; }

        ICheckBox Patriarch { get; }
        ICheckBox Bookmark { get; }

        void SetParentsAvl(bool avail, bool locked);
        void SetFatherAvl(bool avail, bool locked);
        void SetMotherAvl(bool avail, bool locked);
        //void UpdatePortrait(bool totalUpdate);

        void SetPortrait(IImage portrait);
        void SetPortraitAvl(bool avail, bool locked);
    }


    public interface IPersonsFilterDlg : ICommonDialog
    {
        IComboBox SourceCombo { get; }
        IComboBox GroupCombo { get; }
        ITextBox AliveBeforeDate { get; }
        ICheckBox OnlyPatriarchsCheck { get; }
        IComboBox EventValCombo { get; }
        IComboBox ResidenceCombo { get; }
        IComboBox NameCombo { get; }

        void SetLifeRadio(int lifeSel);
        void SetSexRadio(int sexSel);
        int GetLifeRadio();
        int GetSexRadio();
        void SetLifeEnabled(bool value);
    }


    public interface IPortraitSelectDlg : ICommonDialog, IBaseEditor
    {
        GDMMultimediaLink MultimediaLink { get; set; }

        IImageView ImageCtl { get; }
    }


    public interface IQuickSearchDlg : IView, ILocalizable
    {
        ITextBox SearchPattern { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor
    {
        string FastFilter { get; set; }
        GDMRecord ResultRecord { get; set; }

        ITextBox FilterBox { get; }
        IListViewEx RecordsList { get; }

        void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex);
    }


    public interface IRelationshipCalculatorDlg : ICommonDialog
    {
        ILabel Label1 { get; }
        ILabel Label2 { get; }
        ITextBox Person1 { get; }
        ITextBox Person2 { get; }
        ITextBox Result { get; }
    }


    public interface IRepositoryEditDlg : ICommonDialog, IBaseEditor
    {
        GDMRepositoryRecord Repository { get; set; }

        ISheetList NotesList { get; }
        ITextBox Name { get; }
    }


    public interface IResearchEditDlg : ICommonDialog, IBaseEditor
    {
        GDMResearchRecord Research { get; set; }

        ISheetList TasksList { get; }
        ISheetList CommunicationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList NotesList { get; }

        ITextBox Name { get; }
        IComboBox Priority { get; }
        IComboBox Status { get; }
        IDateBox StartDate { get; }
        IDateBox StopDate { get; }
        INumericBox Percent { get; }
    }


    public interface IScriptEditWin : ICommonDialog, ILocalizable
    {
        ITextBox ScriptText { get; }
        ITextBox DebugOutput { get; }

        string FileName { get; set; }
        bool Modified { get; set; }

        bool CheckModified();
    }


    public interface ISexCheckDlg : ICommonDialog
    {
        string IndividualName { get; set; }
        GDMSex Sex { get; set; }
    }


    public interface ISlideshowWin : IWindow, IStatusForm
    {
        void SetImage(IImage image);
        void UpdateControls();
    }


    public interface ISourceCitEditDlg : ICommonDialog, IBaseEditor
    {
        GDMSourceCitation SourceCitation { get; set; }

        ITextBox Page { get; }
        IComboBox Certainty { get; }
        IComboBox Source { get; }

        IDateControl DataDate { get; }
        ITextBox DataText { get; }
    }


    public interface ISourceEditDlg : ICommonDialog, IBaseEditor, IView<GDMSourceRecord, ISourceEditDlg>
    {
        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList RepositoriesList { get; }

        ITextBox ShortTitle { get; }
        ITextBox Author { get; }
        ITextBox Title { get; }
        ITextBox Publication { get; }
        ITextBox Text { get; }
    }


    public interface IStatisticsWin : IWindow
    {
        IGraphControl Graph { get; }
        IListView ListStats { get; }
        IListView Summary { get; }
        IComboBox StatsType { get; }
    }


    public interface ITaskEditDlg : ICommonDialog, IBaseEditor
    {
        GDMTaskRecord Task { get; set; }

        ISheetList NotesList { get; }
        IComboBox Priority { get; }
        IDateBox StartDate { get; }
        IDateBox StopDate { get; }
        IComboBox GoalType { get; }
        ITextBox Goal { get; }
        IButton GoalSelect { get; }
    }


    public interface ITreeChartWin : IChartWindow
    {
        ITreeChart TreeBox { get; }

        void GenChart(TreeChartKind chartKind);
    }


    public interface ITreeFilterDlg : ICommonDialog
    {
        ChartFilter Filter { get; set; }

        ISheetList PersonsList { get; }
        INumericBox YearNum { get; }
        IComboBox SourceCombo { get; }

        int GetCutModeRadio();
        void SetCutModeRadio(int cutMode);
    }


    public interface IUserRefEditDlg : ICommonDialog, IBaseEditor
    {
        GDMUserReference UserRef { get; set; }

        IComboBox Ref { get; }
        IComboBox RefType { get; }
    }



    public interface IFragmentSearchDlg : ICommonDialog, IBaseEditor
    {
        ITreeView GroupsTree { get; }
        ILogChart LogChart { get; }
    }


    public interface IPatriarchsSearchDlg : ICommonDialog, IBaseEditor
    {
        INumericBox MinGensNum { get; }
        ICheckBox WithoutDatesCheck { get; }
        IListView PatriarchsList { get; }
    }


    public interface IPatriarchsViewer : IWindow
    {
    }


    public interface IPlacesManagerDlg : ICommonDialog, IBaseEditor
    {
        IListViewEx PlacesList { get; }
    }


    public interface IRecMergeDlg : ICommonDialog, IBaseEditor
    {
        IMergeControl MergeCtl { get; }
        IButton SkipBtn { get; }
        IProgressBar ProgressBar { get; }
        ICheckBox IndistinctMatchingChk { get; }
        INumericBox NameAccuracyNum { get; }
        ICheckBox BirthYearChk { get; }
        INumericBox YearInaccuracyNum { get; }
    }


    public interface ITreeCheckDlg : ICommonDialog, IBaseEditor
    {
        IListViewEx ChecksList { get; }
    }


    public enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

    public interface ITreeCompareDlg : ICommonDialog, IBaseEditor
    {
        ITextBox ExternalBase { get; }
        ITextBox CompareOutput { get; }

        TreeMatchType GetTreeMatchType();
    }


    public interface ITreeMergeDlg : ICommonDialog, IBaseEditor
    {
        ITextBox UpdateBase { get; }
        ITextBox SyncLog { get; }
    }


    public interface ITreeSplitDlg : ICommonDialog, IBaseEditor
    {
        IListViewEx SelectedList { get; }
        IListViewEx SkippedList { get; }
    }


    public interface IRecordInfoDlg : ICommonDialog, IBaseEditor
    {
        GDMRecord Record { get; set; }

        IHyperView HyperView { get; }
    }
}
