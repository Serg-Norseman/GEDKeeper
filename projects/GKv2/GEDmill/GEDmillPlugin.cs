/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2019-2022 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Reflection;
using BSLib.Design.Graphics;
using BSLib.Design.Handlers;
using GEDmill;
using GKCore;
using GKCore.Interfaces;
using GKCore.Plugins;
using SDIcon = System.Drawing.Icon;

[assembly: AssemblyTitle("GEDmillPlugin")]
[assembly: AssemblyDescription("GEDKeeper's GEDmill plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009 by Alexander Curtis, 2019-2022 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion(GMConfig.SoftwareVersion)]
[assembly: AssemblyCulture("")]

namespace GEDmill
{
    /// <summary>
    /// Plugin's Localizable strings (PLS)
    /// </summary>
    public enum PLS
    {
        LSID_Title,
        LSID_Quit,
        LSID_Finish,
        LSID_Version,
        LSID_Back,
        LSID_Next,
        LSID_Help,
        LSID_Settings,
        LSID_Ok,
        LSID_Cancel,
        LSID_CreatingWebsite,
        LSID_CopyingMultimedia,
        LSID_FrontPageDescription,
        LSID_Keywords,
        LSID_DesktopException,
        LSID_PathException,
        LSID_NotSupportedPictureType,
        LSID_CopyingBackground,
        LSID_CreatingStyleSheet,
        LSID_CreatingIndividualPages,
        LSID_CreatingIndividualsIndex,
        LSID_CreatingSourcePages,
        LSID_CreatingFrontPage,
        LSID_CreatingCDROMFiles,
        LSID_CreatingJSFile,
        LSID_Done,
        LSID_URefNumber,
        LSID_Record,
        LSID_RecordLastChanged,
        LSID_PageCreatedUsingGEDmill,
        LSID_Notes,
        LSID_No,
        LSID_ReturnToMainSite,
        LSID_CreatedOn,
        LSID_FrontPage,
        LSID_MainSite,
        LSID_Index,
        LSID_UnknownImageFormat,
        LSID_Married,
        LSID_PartnerOf,
        LSID_Child,
        LSID_Son,
        LSID_Daughter,
        LSID_Sources,
        LSID_OtherFacts,
        LSID_LifeHistory,
        LSID_Parents,
        LSID_Citations,
        LSID_ExitQuestion,
        LSID_SelectBackImage,
        LSID_SelectFrontImage,
        LSID_PleaseSelectAtLeastOneIndividual,
        LSID_BackgroundMissed,
        LSID_ThereAreNoIndividualsToList,
        LSID_PageDescription,
        LSID_Source,
        LSID_Text,
        LSID_ImageForThisSource,
        LSID_MediaForThisSource,
        LSID_ThisWebsiteContainsRecordsOn,
        LSID_WebsiteCreatedUsingGEDmill,
        LSID_individuals,
        LSID_sources,
        LSID_multimedia,
        LSID_images,
        LSID_KeyIndividuals,
        LSID_previous_child,
        LSID_next_child,
        LSID_death_of,
        LSID_birth_of,
        LSID_most_likely,
        LSID_less_likely,
        LSID_InformationWithheld,
        LSID_also_known_as,
        LSID_MiniTreeDiagram,
        LSID_and,
        LSID_from,
        LSID_to,
        LSID_born,
        LSID_christened,
        LSID_baptised,
        LSID_died,
        LSID_buried,
        LSID_cremated,
        LSID_adopted,
        LSID_bar_mitzvah,
        LSID_bat_mitzvah,
        LSID_blessing,
        LSID_christened_as_adult,
        LSID_confirmed,
        LSID_first_communion,
        LSID_ordained,
        LSID_naturalized,
        LSID_emigrated,
        LSID_immigrated,
        LSID_probate,
        LSID_wrote_will,
        LSID_graduated,
        LSID_retired,
        LSID_other_event,
        LSID_caste,
        LSID_physical_description,
        LSID_educated,
        LSID_ID_number,
        LSID_nationality,
        LSID_number_of_children,
        LSID_number_of_marriages,
        LSID_occupation,
        LSID_property,
        LSID_religion,
        LSID_resident,
        LSID_Social_Security_number,
        LSID_other_fact,
        LSID_never_married,
        LSID_annulment_of_marriage,
        LSID_recorded_in_census,
        LSID_divorced,
        LSID_filing_of_divorce,
        LSID_engagement,
        LSID_publication_of_banns_of_marriage,
        LSID_contract_of_marriage,
        LSID_licence_obtained_for_marriage,
        LSID_settlement_of_marriage,
        LSID_unknown_event,
        LSID_MaritalStatusUnknown,
        LSID_ExcludeIndividuals,
        LSID_ExcludeIndividualsX,
        LSID_RemovePictures,
        LSID_RemovePicturesX,
        LSID_FrontPageFile,
        LSID_NoFrontPage,
        LSID_Include,
        LSID_Name,
        LSID_Born,
        LSID_Died,
        LSID_Id,
        LSID_UserRef,
        LSID_Pics,
        LSID_Individuals,
        LSID_NoSurname,
        LSID_IndexTitle,
        LSID_SiteTitle,
        LSID_ConcealedName,
        LSID_UnknownName,
        LSID_PlaceWord,
        LSID_Repository,
        LSID_UnkProblem,
        LSID_TreeError,
        LSID_ImageSizeProblem,
        LSID_FolderAccessError,
        LSID_FolderCouldNotBeDeleted,
        LSID_FileCouldNotBeDeleted,
        LSID_NoChangesMade,
        LSID_NoMultimediaFilesHidden,
        LSID_Webpages,
        LSID_Images,
        LSID_TreeDiagrams,
        LSID_Advanced,
        LSID_WelcomeSubtitle,
        LSID_RecordsContinue,
        LSID_IndiDescendantsExclude,
        LSID_IndiAncestorsExclude,
        LSID_IndiDescendantsInclude,
        LSID_IndiAncestorsInclude,
        LSID_UnconnectedExclude,
        LSID_Details,
        LSID_IndividualsEveryoneInclude,
        LSID_IndividualsEveryoneExclude,
        LSID_IndividualsAliveExclude,
        LSID_SourceRemovePics,
        LSID_SourcesAllInclude,
        LSID_SourcesAllExclude,
        LSID_PruneRecordsInstructions,
        LSID_PruneRecordsButtons,
        LSID_SelectKey,
        LSID_SelectKeyIndividuals,
        LSID_SelectKeyAdd,
        LSID_SelectKeyDelete,
        LSID_SelectKeyInstructions,
        LSID_ConfigCommentary,
        LSID_IsHtml,
        LSID_ConfigUserLink,
        LSID_ConfigCustomFooter,
        LSID_ConfigStats,
        LSID_ConfigCdrom,
        LSID_ConfigMultiPageIndex,
        LSID_ConfigUserRefInIndex,
        LSID_ConfigMultiPageIndexNumber,
        LSID_ConfigIndexName,
        LSID_ConfigEmail,
        LSID_ConfigBackImageEdit,
        LSID_Browse,
        LSID_ConfigFrontImageEdit,
        LSID_ConfigIndiImageSize,
        LSID_Width,
        LSID_Height,
        LSID_ConfigSourceImageSize,
        LSID_ConfigAllowMultimedia,
        LSID_ConfigRenameOriginals,
        LSID_ConfigKeepOriginals,
        LSID_ConfigNonPictures,
        LSID_ConfigIndiImages,
        LSID_ConfigThumbnailImageSize,
        LSID_ConfigTabSpaces,
        LSID_ConfigNoName,
        LSID_ConfigShowWithheldRecords,
        LSID_ConfigWithheldName,
        LSID_ConfigWithheldNameLabel,
        LSID_ConfigWithheldNameName,
        LSID_ConfigCapNames,
        LSID_ConfigCapEvents,
        LSID_ConfigHideEmails,
        LSID_ConfigOccupationHeadline,
        LSID_ConfigIncludeTreeDiagrams,
        LSID_ConfigTreeDiagramsFormat,
        LSID_ConfigTreeDiagramsFakeBg,
        LSID_ConfigConserveTreeWidth,
        LSID_ConfigKeepSiblingOrder,
        LSID_MiniTreeColours,
        LSID_ConfigMiniTreeColourIndiHighlight,
        LSID_ConfigMiniTreeColourIndiText,
        LSID_ConfigMiniTreeColourIndiBackground,
        LSID_ConfigMiniTreeColourIndiLink,
        LSID_ConfigMiniTreeColourIndiBgConcealed,
        LSID_ConfigMiniTreeColourIndiFgConcealed,
        LSID_ConfigMiniTreeColourIndiShade,
        LSID_ConfigMiniTreeColourBranch,
        LSID_ConfigMiniTreeColourIndiBorder,
        LSID_ConfigUserRecFilename,
        LSID_ConfigSupressBackreferences,
        LSID_ChooseOutput,
        LSID_ChooseOutputInstructions,
        LSID_ChooseOutputContinue,
        LSID_AllDoneShowSite,
        LSID_AllDone,
        LSID_AllDoneThankYou,
        LSID_AllDoneDirectory,
        LSID_FolderNeedsToBeCreated,
        LSID_WillNotAllowToCreateFilesDirectlyOnDesktop,
        LSID_FolderAlreadyExists,
        LSID_DeletingFolderWillNotPreserveAnyExistingFiles,
        LSID_IfFolderContainsOtherFilesTheyWillBeDeletedAlso,
        LSID_FolderNotFound,
        LSID_FolderNameIsIllegal,
        LSID_FolderNameIsTooLong,
        LSID_PathIsReadonlyOrNotEmpty,
        LSID_NotHaveCorrectPermissionsToAccess,
        LSID_FolderNameIsIllegalFormat,
        LSID_FolderNameIsUnsupportedFormat,
    }

    public sealed class Plugin : WidgetPlugin
    {
        private string fDisplayName = "Website Generator (GEDmill)";
        private ILangMan fLangMan;
        private IImage fIcon;

        public override string DisplayName { get { return fDisplayName; } }
        public override ILangMan LangMan { get { return fLangMan; } }
        public override IImage Icon { get { return fIcon; } }
        public override PluginCategory Category { get { return PluginCategory.Report; } }

        private MainForm fForm;

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                CloseForm();
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
            if (fForm != null) {
                fForm = null;
            }
        }

        public override void Execute()
        {
            if (!Host.IsWidgetActive(this)) {
                fForm = new MainForm(this);
                fForm.ShowDialog(); // FIXME
            } else {
                fForm.Close();
            }
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.LSID_Title);

                if (fForm != null)
                    fForm.SetLocale();
            } catch (Exception ex) {
                Logger.WriteError("GEDmillPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
                Assembly assembly = typeof(Plugin).Assembly;
                using (var appIcon = SDIcon.ExtractAssociatedIcon(assembly.Location)) {
                    Image bmp = appIcon.ToBitmap();
                    fIcon = new ImageHandler(bmp);
                }
            } catch (Exception ex) {
                Logger.WriteError("GEDmillPlugin.Startup()", ex);
                result = false;
            }
            return result;
        }

        public override bool Shutdown()
        {
            bool result = true;
            try {
                CloseForm();
            } catch (Exception ex) {
                Logger.WriteError("GEDmillPlugin.Shutdown()", ex);
                result = false;
            }
            return result;
        }

        public override void BaseChanged(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
        }
    }
}
