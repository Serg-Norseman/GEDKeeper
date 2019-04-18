/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using GKCommon.GEDCOM;
using GKCore.Charts;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.MVP.Controls;
using GKCore.Options;
using GKCore.Types;

namespace GKCore.MVP.Views
{
    public interface IAboutDlg : ICommonDialog, IView
    {
    }


    public interface IAddressEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMAddress Address { get; set; }

        ITextBoxHandler Country { get; }
        ITextBoxHandler State { get; }
        ITextBoxHandler City { get; }
        ITextBoxHandler PostalCode { get; }
        ITextBoxHandler AddressLine { get; }

        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IAssociationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMAssociation Association { get; set; }

        ITextBoxHandler Person { get; }
        IComboBoxHandler Relation { get; }
    }


    public interface IBaseWindowView : IBaseWindow, IView
    {
        ITabControl RecordTabs { get; }
        IMenuItem ReportsItem { get; }
        IMenuItem PluginsItem { get; }

        bool CheckModified();
    }


    public interface ICircleChartWin : IChartWindow, IView
    {
        ICircleChart CircleChart { get; }
    }


    public interface ICommonFilterDlg : ICommonDialog, IView
    {
        IFilterGridView FilterGrid { get; }
    }


    public interface ICommunicationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMCommunicationRecord Communication { get; set; }

        ITextBoxHandler Corresponder { get; }
        IComboBoxHandler CorrType { get; }
        ITextBoxHandler Date { get; }
        IComboBoxHandler Dir { get; }
        ITextBoxHandler Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
    }


    public interface IDayTipsDlg : ICommonDialog, IView
    {
        bool ShowTipsChecked { get; set; }

        void Init(string caption, bool showTipsChecked, StringList tips);

        ILabelHandler TitleLabel { get; }
        ITextBoxHandler TipText { get; }
        IButtonHandler NextButton { get; }
    }


    public interface IEventEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMCustomEvent Event { get; set; }

        IComboBoxHandler EventType { get; }
        IComboBoxHandler EventDateType { get; }

        ICheckBoxHandler Date1BC { get; }
        ICheckBoxHandler Date2BC { get; }

        IComboBoxHandler Date1Calendar { get; }
        IComboBoxHandler Date2Calendar { get; }

        IDateBoxHandler Date1 { get; }
        IDateBoxHandler Date2 { get; }

        IComboBoxHandler Attribute { get; }
        ITextBoxHandler Place { get; }
        ITextBoxHandler EventName { get; }
        ITextBoxHandler Cause { get; }
        ITextBoxHandler Agency { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }

        void SetLocationMode(bool active);
    }


    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMFamilyRecord Family { get; set; }

        void SetTarget(TargetMode targetType, GEDCOMIndividualRecord target);
        void LockEditor(bool locked);
        void SetHusband(string value);
        void SetWife(string value);

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ChildrenList { get; }
        ISheetList EventsList { get; }

        IComboBoxHandler MarriageStatus { get; }
        IComboBoxHandler Restriction { get; }
        ITextBoxHandler Husband { get; }
        ITextBoxHandler Wife { get; }
    }


    public interface IFilePropertiesDlg : ICommonDialog, IBaseEditor, IView
    {
        IListView RecordStats { get; }

        ITextBoxHandler Language { get; }
        ITextBoxHandler Name { get; }
        ITextBoxHandler Address { get; }
        ITextBoxHandler Tel { get; }
    }


    public interface IGroupEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMGroupRecord Group { get; set; }

        ITextBoxHandler Name { get; }

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList MembersList { get; }
    }


    public interface ILanguageEditDlg : ICommonDialog, IView
    {
        GEDCOMLanguageID LanguageID { get; set; }

        IComboBoxHandler LanguageCombo { get; }
    }


    public interface ILanguageSelectDlg : ICommonDialog, IView
    {
        int SelectedLanguage { get; set; }

        IListView LanguagesList { get; }
    }


    public interface ILocationEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMLocationRecord LocationRecord { get; set; }

        IMapBrowser MapBrowser { get; }
        ISheetList MediaList { get; }
        ISheetList NotesList { get; }
        IListView GeoCoordsList { get; }
        ITextBoxHandler Name { get; }
        ITextBoxHandler Latitude { get; }
        ITextBoxHandler Longitude { get; }
    }


    public interface IMapsViewerWin : IWindow, IView
    {
        IMapBrowser MapBrowser { get; }
        IComboBoxHandler PersonsCombo { get; }
        ITreeViewHandler PlacesTree { get; }
        IButtonHandler SelectPlacesBtn { get; }
        ICheckBoxHandler BirthCheck { get; }
        ICheckBoxHandler DeathCheck { get; }
        ICheckBoxHandler ResidenceCheck { get; }
        ICheckBoxHandler LinesVisibleCheck { get; }
        IRadioButtonHandler TotalRadio { get; }
        IRadioButtonHandler SelectedRadio { get; }

        ITVNode FindTreeNode(string place);
    }


    public interface IMediaEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMMultimediaRecord MediaRec { get; set; }

        ISheetList NotesList { get; }
        ISheetList SourcesList { get; }

        IComboBoxHandler MediaType { get; }
        IComboBoxHandler StoreType { get; }
        ITextBoxHandler Name { get; }
        ITextBoxHandler File { get; }
        IButtonHandler FileSelectButton { get; }
    }


    public interface IMediaViewerWin : IWindow, IView
    {
        GEDCOMFileReferenceWithTitle FileRef { get; set; }

        void SetViewImage(IImage img, GEDCOMFileReferenceWithTitle fileRef);
        void SetViewMedia(string mediaFile);
        void SetViewText(string text);
        void SetViewRTF(string text);
        void SetViewHTML(Stream stm);
        void DisposeViewControl();
    }


    public interface INameEditDlg : ICommonDialog, IView
    {
        NameEntry IName { get; set; }

        ITextBoxHandler Name { get; }
        ITextBoxHandler FPatr { get; }
        ITextBoxHandler MPatr { get; }
        IComboBoxHandler SexCombo { get; }
    }


    public interface INoteEdit : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMNoteRecord NoteRecord { get; set; }

        ITextBoxHandler Note { get; }
    }


    public interface INoteEditDlg : INoteEdit
    {
    }

    public interface INoteEditDlgEx : INoteEdit
    {
    }


    public interface IOrganizerWin : ICommonDialog, IView
    {
        ISheetList AdrList { get; }
        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }


    public interface IOptionsDlg : ICommonDialog, IView
    {
        void SetPage(OptionsPage page);
    }


    public interface IParentsEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMChildToFamilyLink Link { get; set; }
        GEDCOMIndividualRecord Person { get; set; }

        ITextBoxHandler Father { get; }
        ITextBoxHandler Mother { get; }
        ITextBoxHandler ChildName { get; }
        IComboBoxHandler LinkageTypeCombo { get; }

        void SetParentsAvl(bool avail);
        void SetFatherAvl(bool avail);
        void SetMotherAvl(bool avail);
    }


    public interface IPersonalNameEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMPersonalName PersonalName { get; set; }

        ILabelHandler SurnameLabel { get; }
        ITextBoxHandler Surname { get; }
        ITextBoxHandler Name { get; }
        ITextBoxHandler Patronymic { get; }
        IComboBoxHandler NameType { get; }
        ITextBoxHandler NamePrefix { get; }
        ITextBoxHandler Nickname { get; }
        ITextBoxHandler SurnamePrefix { get; }
        ITextBoxHandler NameSuffix { get; }
        ITextBoxHandler MarriedSurname { get; }
        IComboBoxHandler Language { get; }
    }


    public interface IPersonEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMIndividualRecord Person { get; set; }
        GEDCOMIndividualRecord Target { get; set; }
        TargetMode TargetMode { get; set; }
        void SetNeedSex(GEDCOMSex needSex);

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
        ITextBoxHandler Father { get; }
        ITextBoxHandler Mother { get; }
        ITextBoxHandler Surname { get; }
        ITextBoxHandler Name { get; }
        IComboBoxHandler Patronymic { get; }
        ITextBoxHandler NamePrefix { get; }
        ITextBoxHandler Nickname { get; }
        ITextBoxHandler SurnamePrefix { get; }
        ITextBoxHandler NameSuffix { get; }
        ITextBoxHandler MarriedSurname { get; }

        ILabelHandler SurnameLabel { get; }
        IComboBoxHandler RestrictionCombo { get; }
        IComboBoxHandler SexCombo { get; }

        ICheckBoxHandler Patriarch { get; }
        ICheckBoxHandler Bookmark { get; }

        void SetParentsAvl(bool avail, bool locked);
        void SetFatherAvl(bool avail, bool locked);
        void SetMotherAvl(bool avail, bool locked);
        //void UpdatePortrait(bool totalUpdate);

        void SetPortrait(IImage portrait);
        void SetPortraitAvl(bool avail, bool locked);
    }


    public interface IPersonsFilterDlg : ICommonDialog, IView
    {
        IComboBoxHandler SourceCombo { get; }
        IComboBoxHandler GroupCombo { get; }
        ITextBoxHandler AliveBeforeDate { get; }
        ICheckBoxHandler OnlyPatriarchsCheck { get; }
        IComboBoxHandler EventValCombo { get; }
        IComboBoxHandler ResidenceCombo { get; }
        IComboBoxHandler NameCombo { get; }

        void SetLifeRadio(int lifeSel);
        void SetSexRadio(int sexSel);
        int GetLifeRadio();
        int GetSexRadio();
        void SetLifeEnabled(bool value);
    }


    public interface IPortraitSelectDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMMultimediaLink MultimediaLink { get; set; }

        IImageView ImageCtl { get; }
    }


    public interface IQuickSearchDlg : IView, ILocalization
    {
        ITextBoxHandler SearchPattern { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor, IView
    {
        string FastFilter { get; set; }
        TargetMode TargetMode { get; set; }
        GEDCOMIndividualRecord TargetIndividual { get; set; }
        GEDCOMSex NeedSex { get; set; }
        GEDCOMRecord ResultRecord { get; set; }

        IListView RecordsList { get; }
    }


    public interface IRelationshipCalculatorDlg : ICommonDialog, IView
    {
        ILabelHandler Label1 { get; }
        ILabelHandler Label2 { get; }
        ITextBoxHandler Person1 { get; }
        ITextBoxHandler Person2 { get; }
        ITextBoxHandler Result { get; }
    }


    public interface IRepositoryEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMRepositoryRecord Repository { get; set; }

        ISheetList NotesList { get; }
        ITextBoxHandler Name { get; }
    }


    public interface IResearchEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMResearchRecord Research { get; set; }

        ISheetList TasksList { get; }
        ISheetList CommunicationsList { get; }
        ISheetList GroupsList { get; }
        ISheetList NotesList { get; }

        ITextBoxHandler Name { get; }
        IComboBoxHandler Priority { get; }
        IComboBoxHandler Status { get; }
        ITextBoxHandler StartDate { get; }
        ITextBoxHandler StopDate { get; }
        INumericBoxHandler Percent { get; }
    }


    public interface IScriptEditWin : ICommonDialog, ILocalization, IView
    {
        ITextBoxHandler ScriptText { get; }
        ITextBoxHandler DebugOutput { get; }

        string FileName { get; set; }
        bool Modified { get; set; }

        bool CheckModified();
    }


    public interface ISexCheckDlg : ICommonDialog, IView
    {
        string IndividualName { get; set; }
        GEDCOMSex Sex { get; set; }
    }


    public interface ISlideshowWin : IWindow, IStatusForm, IView
    {
        void SetImage(IImage image);
        void UpdateControls();
    }


    public interface ISourceCitEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMSourceCitation SourceCitation { get; set; }

        ITextBoxHandler Page { get; }
        IComboBoxHandler Certainty { get; }
        IComboBoxHandler Source { get; }
    }


    public interface ISourceEditDlg : ICommonDialog, IBaseEditor, IView<GEDCOMSourceRecord, ISourceEditDlg>
    {
        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList RepositoriesList { get; }

        ITextBoxHandler ShortTitle { get; }
        ITextBoxHandler Author { get; }
        ITextBoxHandler Title { get; }
        ITextBoxHandler Publication { get; }
        ITextBoxHandler Text { get; }
    }


    public interface IStatisticsWin : IWindow, IView
    {
        IGraphControl Graph { get; }
        IListView ListStats { get; }
        IListView Summary { get; }
        IComboBoxHandler StatsType { get; }
    }


    public interface ITaskEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMTaskRecord Task { get; set; }

        ISheetList NotesList { get; }
        IComboBoxHandler Priority { get; }
        ITextBoxHandler StartDate { get; }
        ITextBoxHandler StopDate { get; }
        IComboBoxHandler GoalType { get; }
        ITextBoxHandler Goal { get; }
        IButtonHandler GoalSelect { get; }
    }


    public interface ITreeChartWin : IChartWindow, IView
    {
        ITreeChartBox TreeBox { get; }
        TreeChartKind ChartKind { get; set; }
    }


    public interface ITreeFilterDlg : ICommonDialog, IView
    {
        ChartFilter Filter { get; set; }

        ISheetList PersonsList { get; }
        INumericBoxHandler YearNum { get; }
        IComboBoxHandler SourceCombo { get; }

        int GetCutModeRadio();
        void SetCutModeRadio(int cutMode);
    }


    public interface IUserRefEditDlg : ICommonDialog, IBaseEditor, IView
    {
        GEDCOMUserReference UserRef { get; set; }

        IComboBoxHandler Ref { get; }
        IComboBoxHandler RefType { get; }
    }



    public interface IFragmentSearchDlg : ICommonDialog, IBaseEditor, IView
    {
        ITreeViewHandler GroupsTree { get; }
        ILogChart LogChart { get; }
    }


    public interface IPatriarchsSearchDlg : ICommonDialog, IBaseEditor, IView
    {
        INumericBoxHandler MinGensNum { get; }
        ICheckBoxHandler WithoutDatesCheck { get; }
        IListView PatriarchsList { get; }
    }


    public interface IPatriarchsViewer : IWindow, IView
    {
    }


    public interface IPlacesManagerDlg : ICommonDialog, IBaseEditor, IView
    {
        IListView PlacesList { get; }
    }


    public interface IRecMergeDlg : ICommonDialog, IBaseEditor, IView
    {
        IMergeControl MergeCtl { get; }
        IButtonHandler SkipBtn { get; }
        IProgressBarHandler ProgressBar { get; }
        ICheckBoxHandler IndistinctMatchingChk { get; }
        INumericBoxHandler NameAccuracyNum { get; }
        ICheckBoxHandler BirthYearChk { get; }
        INumericBoxHandler YearInaccuracyNum { get; }
    }


    public interface ITreeCheckDlg : ICommonDialog, IBaseEditor, IView
    {
        IListView ChecksList { get; }
    }


    public enum TreeMatchType { tmtInternal, tmtExternal, tmtAnalysis }

    public interface ITreeCompareDlg : ICommonDialog, IBaseEditor, IView
    {
        ITextBoxHandler ExternalBase { get; }
        ITextBoxHandler CompareOutput { get; }

        TreeMatchType GetTreeMatchType();
    }


    public interface ITreeMergeDlg : ICommonDialog, IBaseEditor, IView
    {
        ITextBoxHandler UpdateBase { get; }
        ITextBoxHandler SyncLog { get; }
    }


    public interface ITreeSplitDlg : ICommonDialog, IBaseEditor, IView
    {
        IListView SelectedList { get; }
        IListView SkippedList { get; }
    }
}
