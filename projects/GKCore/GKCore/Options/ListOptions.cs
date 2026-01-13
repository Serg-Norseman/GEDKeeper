/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GDModel;
using GKCore.Lists;

namespace GKCore.Options
{
    public class ListOptionsException : Exception
    {
        public ListOptionsException(string message) : base(message)
        {
        }
    }

    /// <summary>
    /// New items can only be added to the end.
    /// </summary>
    public enum GKListType
    {
        ltNone,

        // records
        rtIndividual,
        rtFamily,
        rtNote,
        rtMultimedia,
        rtSource,
        rtRepository,
        rtGroup,
        rtResearch,
        rtTask,
        rtCommunication,
        rtLocation,
        rtSubmission,
        rtSubmitter,

        // all record substructures
        stMediaLinks,
        stNoteLinks,
        stSourceCitations,
        stUserRefs,

        // rtIndividual + rtFamily substructures
        stEvents,
        stChildren,

        // rtIndividual substructures
        stIndividualAssociations,
        stIndividualGroups,
        stIndividualNames,
        stIndividualParents,
        stIndividualSpouses,
        stIndividualDNATests,

        // rtFamily substructures

        // rtSource
        stSourceRepositories,

        // RepositoryCitation
        stRepoCitCallNumbers,

        // rtGroup
        stGroupMembers,

        // rtResearch
        stResearchTasks,
        stResearchCommunications,
        stResearchGroups,

        // rtLocation
        stLocationLinks,
        stLocationNames,

        // services
        stEventDefs,

        // rtMultimedia substructures
        stMediaFiles,

        ltFirst = rtIndividual,
        ltLast = stMediaFiles,
    }


    /// <summary>
    ///
    /// </summary>
    public class ListOptions : IOptions
    {
        public static readonly string[] ListTypeNames = new string[] {
            "",

            // records
            "Individuals",
            "Families",
            "Notes",
            "Multimedia",
            "Sources",
            "Repositories",
            "Groups",
            "Researches",
            "Tasks",
            "Communications",
            "Locations",
            "Submissions",
            "Submitters",

            // all record substructures
            "MediaLinks",
            "NoteLinks",
            "SourceCitations",
            "UserRefs",

            // rtIndividual + rtFamily substructures
            "Events",
            "Children",

            // rtIndividual substructures
            "IndividualAssociations",
            "IndividualGroups",
            "IndividualNames",
            "IndividualParents",
            "IndividualSpouses",
            "IndividualDNATests",

            // rtFamily substructures

            // rtSource
            "SourceRepositories",

            // RepositoryCitation
            "RepoCitCallNumbers",

            // rtGroup
            "GroupMembers",

            // rtResearch
            "ResearchTasks",
            "ResearchCommunications",
            "ResearchGroups",

            // rtLocation
            "LocationLinks",
            "LocationNames",

            // services
            "EventDefs",

            // rtMultimedia substructures
            "MediaFiles",
        };

        private readonly ListColumns fColumns;
        private readonly string fName;

        public ListColumns Columns
        {
            get { return fColumns; }
        }

        public int SortColumn { get; set; }

        public int SplitterPosition { get; set; }

        public ListOptions(GKListType listType, ListColumns listColumns)
        {
            fName = ListTypeNames[(int)listType] + "List";
            fColumns = listColumns;

            ResetDefaults();
        }

        public void ResetDefaults()
        {
            SortColumn = 0;
            SplitterPosition = 300;

            fColumns.ResetDefaults();
        }

        public void Assign(IOptions source)
        {
            ListOptions srcOptions = source as ListOptions;
            if (srcOptions == null) return;

            SortColumn = srcOptions.SortColumn;
            SplitterPosition = srcOptions.SplitterPosition;
        }

        public void LoadFromFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            try {
                SortColumn = iniFile.ReadInteger(fName, "SortColumn", 0);
                SplitterPosition = iniFile.ReadInteger(fName, "SplitterPosition", 300);

                if (optsVersion >= 3) {
                    fColumns.LoadFromFile(iniFile, fName, optsVersion);
                }
            } catch (Exception) {
                throw new ListOptionsException("Error loading ListOptions");
            }
        }

        public void SaveToFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            iniFile.WriteInteger(fName, "SortColumn", SortColumn);
            iniFile.WriteInteger(fName, "SplitterPosition", SplitterPosition);

            if (optsVersion >= 3) {
                fColumns.SaveToFile(iniFile, fName, optsVersion);
            }
        }
    }

    public sealed class ListOptionsCollection
    {
        private readonly ListOptions[] fOptions;

        public ListOptions this[GDMRecordType rt]
        {
            get { return fOptions[(int)rt]; }
        }

        public ListOptions this[GKListType lt]
        {
            get { return fOptions[(int)lt]; }
        }

        public ListOptionsCollection()
        {
            fOptions = new ListOptions[(int)GKListType.ltLast + 1];
            for (var lt = GKListType.ltFirst; lt <= GKListType.ltLast; lt++) {
                fOptions[(int)lt] = new ListOptions(lt, GetListColumns(lt));
            }
        }

        /*private ListColumns GetListColumns(GDMRecordType recType)
        {
            int id = (int)recType;
            return GetListColumns((GKListType)id);
        }*/

        private ListColumns GetListColumns(GKListType listType)
        {
            ListColumns result;

            switch (listType) {
                case GKListType.rtIndividual:
                    result = IndividualListModel.CreateListColumns();
                    break;

                case GKListType.rtFamily:
                    result = FamilyListModel.CreateListColumns();
                    break;

                case GKListType.rtNote:
                    result = NoteListModel.CreateListColumns();
                    break;

                case GKListType.rtMultimedia:
                    result = MultimediaListModel.CreateListColumns();
                    break;

                case GKListType.rtSource:
                    result = SourceListModel.CreateListColumns();
                    break;

                case GKListType.rtRepository:
                    result = RepositoryListModel.CreateListColumns();
                    break;

                case GKListType.rtGroup:
                    result = GroupListModel.CreateListColumns();
                    break;

                case GKListType.rtResearch:
                    result = ResearchListModel.CreateListColumns();
                    break;

                case GKListType.rtTask:
                    result = TaskListModel.CreateListColumns();
                    break;

                case GKListType.rtCommunication:
                    result = CommunicationListModel.CreateListColumns();
                    break;

                case GKListType.rtLocation:
                    result = LocationListModel.CreateListColumns();
                    break;


                case GKListType.stMediaLinks:
                    result = MediaLinksListModel.CreateListColumns();
                    break;

                case GKListType.stNoteLinks:
                    result = NoteLinksListModel.CreateListColumns();
                    break;

                case GKListType.stSourceCitations:
                    result = SourceCitationsListModel.CreateListColumns();
                    break;

                case GKListType.stUserRefs:
                    result = URefsListModel.CreateListColumns();
                    break;

                case GKListType.stEvents:
                    result = EventsListModel.CreateListColumns();
                    break;

                case GKListType.stChildren:
                    result = ChildrenListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualAssociations:
                    result = AssociationsListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualGroups:
                    result = IndiGroupsListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualNames:
                    result = IndiNamesListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualParents:
                    result = IndiParentsListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualSpouses:
                    result = IndiSpousesListModel.CreateListColumns();
                    break;

                case GKListType.stIndividualDNATests:
                    result = DNATestsListModel.CreateListColumns();
                    break;

                case GKListType.stSourceRepositories:
                    result = RepositoryCitationsListModel.CreateListColumns();
                    break;

                case GKListType.stRepoCitCallNumbers:
                    result = CallNumbersListModel.CreateListColumns();
                    break;

                case GKListType.stGroupMembers:
                    result = GroupMembersListModel.CreateListColumns();
                    break;

                case GKListType.stResearchTasks:
                    result = ResTasksListModel.CreateListColumns();
                    break;

                case GKListType.stResearchCommunications:
                    result = ResCommunicationsListModel.CreateListColumns();
                    break;

                case GKListType.stResearchGroups:
                    result = ResGroupsListModel.CreateListColumns();
                    break;

                case GKListType.stLocationLinks:
                    result = LocationLinksListModel.CreateListColumns();
                    break;

                case GKListType.stLocationNames:
                    result = LocationNamesListModel.CreateListColumns();
                    break;

                case GKListType.stEventDefs:
                    result = EventDefsListModel.CreateListColumns();
                    break;

                case GKListType.stMediaFiles:
                    result = MediaFilesListModel.CreateListColumns();
                    break;

                default:
                    result = new ListColumns(GKListType.ltNone);
                    break;
            }

            result.ResetDefaults();

            return result;
        }

        public void LoadFromFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            for (var lt = GKListType.ltFirst; lt <= GKListType.ltLast; lt++) {
                fOptions[(int)lt].LoadFromFile(iniFile, optsVersion);
            }
        }

        public void SaveToFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException(nameof(iniFile));

            for (var lt = GKListType.ltFirst; lt <= GKListType.ltLast; lt++) {
                fOptions[(int)lt].SaveToFile(iniFile, optsVersion);
            }
        }
    }
}
