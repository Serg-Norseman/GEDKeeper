/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Reflection;
using GEDmill;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Plugins;
using GKUI.Platform.Handlers;
using GKCore.Design;
using GKCore.Locales;

#if !ETO
using System.Drawing;
using SDIcon = System.Drawing.Icon;
#else
#endif

[assembly: AssemblyTitle("GEDmillPlugin")]
[assembly: AssemblyDescription("GEDKeeper's GEDmill plugin")]
[assembly: AssemblyProduct("GEDKeeper")]
[assembly: AssemblyCopyright("Copyright © 2009 by Alexander Curtis, 2019-2023 by Sergey V. Zhdanovskih")]
[assembly: AssemblyVersion(GMConfig.SoftwareVersion)]
[assembly: AssemblyCulture("")]

namespace GEDmill
{
    /// <summary>
    /// Plugin's Localizable strings (PLS)
    /// </summary>
    public enum PLS
    {
        Title = 1,
        Quit,
        Finish,
        Version,
        Back,
        Next,
        Help,
        Settings,
        Ok,
        Cancel,
        CreatingWebsite,
        CopyingMultimedia,
        FrontPageDescription,
        Keywords,
        DesktopException,
        PathException,
        NotSupportedPictureType,
        CopyingBackground,
        CreatingStyleSheet,
        CreatingIndividualPages,
        CreatingIndividualsIndex,
        CreatingSourcePages,
        CreatingFrontPage,
        CreatingJSFile,
        Done,
        URefNumber,
        Record,
        RecordLastChanged,
        PageCreatedUsingGEDmill,
        Notes,
        No,
        ReturnToMainSite,
        CreatedOn,
        FrontPage,
        MainSite,
        Index,
        UnknownImageFormat,
        Married,
        PartnerOf,
        Child,
        Son,
        Daughter,
        Sources,
        OtherFacts,
        LifeHistory,
        Parents,
        Citations,
        ExitQuestion,
        SelectBackImage,
        SelectFrontImage,
        PleaseSelectAtLeastOneIndividual,
        BackgroundMissed,
        ThereAreNoIndividualsToList,
        PageDescription,
        Source,
        Text,
        ImageForThisSource,
        MediaForThisSource,
        ThisWebsiteContainsRecordsOn,
        WebsiteCreatedUsingGEDmill,
        individuals,
        sources,
        multimedia,
        images,
        KeyIndividuals,
        previous_child,
        next_child,
        death_of,
        birth_of,
        most_likely,
        less_likely,
        InformationWithheld,
        also_known_as,
        MiniTreeDiagram,
        and,
        from,
        to,
        born,
        christened,
        baptised,
        died,
        buried,
        cremated,
        adopted,
        bar_mitzvah,
        bat_mitzvah,
        blessing,
        christened_as_adult,
        confirmed,
        first_communion,
        ordained,
        naturalized,
        emigrated,
        immigrated,
        probate,
        wrote_will,
        graduated,
        retired,
        other_event,
        caste,
        physical_description,
        educated,
        ID_number,
        nationality,
        number_of_children,
        number_of_marriages,
        occupation,
        property,
        religion,
        resident,
        Social_Security_number,
        other_fact,
        never_married,
        annulment_of_marriage,
        recorded_in_census,
        divorced,
        filing_of_divorce,
        engagement,
        publication_of_banns_of_marriage,
        contract_of_marriage,
        licence_obtained_for_marriage,
        settlement_of_marriage,
        unknown_event,
        MaritalStatusUnknown,
        ExcludeIndividuals,
        ExcludeIndividualsX,
        RemovePictures,
        RemovePicturesX,
        FrontPageFile,
        NoFrontPage,
        Include,
        Name,
        Born,
        Died,
        Id,
        UserRef,
        Pics,
        Individuals,
        NoSurname,
        IndexTitle,
        SiteTitle,
        ConcealedName,
        UnknownName,
        PlaceWord,
        Repository,
        UnkProblem,
        TreeError,
        ImageSizeProblem,
        FolderAccessError,
        FolderCouldNotBeDeleted,
        FileCouldNotBeDeleted,
        NoChangesMade,
        NoMultimediaFilesHidden,
        Webpages,
        Images,
        TreeDiagrams,
        Advanced_unused,
        WelcomeSubtitle,
        RecordsContinue,
        IndiDescendantsExclude,
        IndiAncestorsExclude,
        IndiDescendantsInclude,
        IndiAncestorsInclude,
        UnconnectedExclude,
        Details,
        IndividualsEveryoneInclude,
        IndividualsEveryoneExclude,
        IndividualsAliveExclude,
        SourceRemovePics,
        SourcesAllInclude,
        SourcesAllExclude,
        PruneRecordsInstructions,
        PruneRecordsButtons,
        SelectKey,
        SelectKeyIndividuals,
        SelectKeyAdd,
        SelectKeyDelete,
        SelectKeyInstructions,
        ConfigCommentary,
        IsHtml,
        ConfigUserLink,
        ConfigCustomFooter,
        ConfigStats,
        ConfigMultiPageIndex,
        ConfigUserRefInIndex_unused,
        ConfigMultiPageIndexNumber,
        ConfigIndexName,
        ConfigEmail,
        ConfigBackImageEdit,
        Browse,
        ConfigFrontImageEdit,
        ConfigIndiImageSize,
        Width,
        Height,
        ConfigSourceImageSize,
        ConfigAllowMultimedia,
        ConfigRenameOriginals,
        ConfigKeepOriginals,
        ConfigNonPictures,
        ConfigIndiImages,
        ConfigThumbnailImageSize,
        ConfigTabSpaces_unused,
        ConfigNoName,
        ConfigShowWithheldRecords,
        ConfigWithheldName,
        ConfigWithheldNameLabel,
        ConfigWithheldNameName,
        ConfigCapSurnames,
        ConfigCapEvents,
        ConfigHideEmails,
        ConfigOccupationHeadline,
        ConfigIncludeTreeDiagrams,
        ConfigTreeDiagramsFakeBg,
        ConfigConserveTreeWidth,
        ConfigKeepSiblingOrder_unused,
        MiniTreeColors,
        ConfigMiniTreeColorIndiHighlight,
        ConfigMiniTreeColorIndiText,
        ConfigMiniTreeColorIndiBackground,
        ConfigMiniTreeColorIndiLink,
        ConfigMiniTreeColorIndiBgConcealed,
        ConfigMiniTreeColorIndiFgConcealed,
        ConfigMiniTreeColorIndiShade,
        ConfigMiniTreeColorBranch,
        ConfigMiniTreeColorIndiBorder,
        ConfigUserRecFilename_unused,
        ConfigSupressBackreferences,
        ChooseOutput,
        ChooseOutputInstructions,
        ChooseOutputContinue,
        AllDoneShowSite,
        AllDone,
        AllDoneThankYou,
        AllDoneDirectory,
        FolderNeedsToBeCreated,
        WillNotAllowToCreateFilesDirectlyOnDesktop,
        FolderAlreadyExists,
        DeletingFolderWillNotPreserveAnyExistingFiles,
        IfFolderContainsOtherFilesTheyWillBeDeletedAlso,
        FolderNotFound,
        FolderNameIsIllegal,
        FolderNameIsTooLong,
        PathIsReadonlyOrNotEmpty,
        NotHaveCorrectPermissionsToAccess,
        FolderNameIsIllegalFormat,
        FolderNameIsUnsupportedFormat,
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

#if !ETO
        private MainForm fForm;
#endif

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                CloseForm();
            }
            base.Dispose(disposing);
        }

        internal void CloseForm()
        {
#if !ETO
            if (fForm != null) {
                fForm = null;
            }
#endif
        }

        public override void Execute()
        {
#if !ETO
            if (!Host.IsWidgetActive(this)) {
                fForm = new MainForm(this);
                fForm.ShowDialog(); // FIXME
            } else {
                fForm.Close();
            }
#endif
        }

        public override void OnLanguageChange()
        {
            try {
                fLangMan = Host.CreateLangMan(this);
                fDisplayName = fLangMan.LS(PLS.Title);

#if !ETO
                if (fForm != null)
                    fForm.SetLocale();
#endif
            } catch (Exception ex) {
                Logger.WriteError("GEDmillPlugin.OnLanguageChange()", ex);
            }
        }

        public override bool Startup(IHost host)
        {
            bool result = base.Startup(host);
            try {
#if !ETO
                Assembly assembly = typeof(Plugin).Assembly;
                using (var appIcon = SDIcon.ExtractAssociatedIcon(assembly.Location)) {
                    Image bmp = appIcon.ToBitmap();
                    fIcon = new ImageHandler(bmp);
                }
#endif
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
#if !ETO
            if (fForm != null) {
                fForm.BaseChanged(baseWin);
            }
#endif
        }

        public override void BaseClosed(IBaseWindow baseWin)
        {
#if !ETO
            if (fForm != null) {
                fForm.BaseChanged(null);
            }
#endif
        }
    }
}
