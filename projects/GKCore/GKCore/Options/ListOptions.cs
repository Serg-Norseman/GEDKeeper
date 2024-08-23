/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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
using BSLib;
using GDModel;
using GKCore.Interfaces;
using GKCore.Lists;

namespace GKCore.Options
{
    public class ListOptionsException : Exception
    {
        public ListOptionsException(string message) : base(message)
        {
        }
    }

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

        ltFirst = rtIndividual,
        ltLast = stEventDefs,
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
        };

        private readonly IListColumns fColumns;
        private readonly string fName;

        public IListColumns Columns
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
                throw new ArgumentNullException("iniFile");

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
                throw new ArgumentNullException("iniFile");

            iniFile.WriteInteger(fName, "SortColumn", SortColumn);
            iniFile.WriteInteger(fName, "SplitterPosition", SplitterPosition);

            if (optsVersion >= 3) {
                fColumns.SaveToFile(iniFile, fName, optsVersion);
            }
        }
    }

    public sealed class ListOptionsCollection
    {
        private ListOptions[] fOptions;

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
            switch (listType) {
                case GKListType.rtIndividual:
                    return IndividualListModel.CreateListColumns();

                case GKListType.rtFamily:
                    return FamilyListModel.CreateListColumns();

                case GKListType.rtNote:
                    return NoteListModel.CreateListColumns();

                case GKListType.rtMultimedia:
                    return MultimediaListModel.CreateListColumns();

                case GKListType.rtSource:
                    return SourceListModel.CreateListColumns();

                case GKListType.rtRepository:
                    return RepositoryListModel.CreateListColumns();

                case GKListType.rtGroup:
                    return GroupListModel.CreateListColumns();

                case GKListType.rtResearch:
                    return ResearchListModel.CreateListColumns();

                case GKListType.rtTask:
                    return TaskListModel.CreateListColumns();

                case GKListType.rtCommunication:
                    return CommunicationListModel.CreateListColumns();

                case GKListType.rtLocation:
                    return LocationListModel.CreateListColumns();


                case GKListType.stMediaLinks:
                    return MediaLinksListModel.CreateListColumns();

                case GKListType.stNoteLinks:
                    return NoteLinksListModel.CreateListColumns();

                case GKListType.stSourceCitations:
                    return SourceCitationsListModel.CreateListColumns();

                case GKListType.stUserRefs:
                    return URefsListModel.CreateListColumns();

                case GKListType.stEvents:
                    return EventsListModel.CreateListColumns();

                case GKListType.stChildren:
                    return ChildrenListModel.CreateListColumns();

                case GKListType.stIndividualAssociations:
                    return AssociationsListModel.CreateListColumns();

                case GKListType.stIndividualGroups:
                    return IndiGroupsListModel.CreateListColumns();

                case GKListType.stIndividualNames:
                    return IndiNamesListModel.CreateListColumns();

                case GKListType.stIndividualParents:
                    return IndiParentsListModel.CreateListColumns();

                case GKListType.stIndividualSpouses:
                    return IndiSpousesListModel.CreateListColumns();

                case GKListType.stSourceRepositories:
                    return RepositoryCitationsListModel.CreateListColumns();

                case GKListType.stRepoCitCallNumbers:
                    return CallNumbersListModel.CreateListColumns();

                case GKListType.stGroupMembers:
                    return GroupMembersListModel.CreateListColumns();

                case GKListType.stResearchTasks:
                    return ResTasksListModel.CreateListColumns();

                case GKListType.stResearchCommunications:
                    return ResCommunicationsListModel.CreateListColumns();

                case GKListType.stResearchGroups:
                    return ResGroupsListModel.CreateListColumns();

                case GKListType.stLocationLinks:
                    return LocationLinksListModel.CreateListColumns();

                case GKListType.stLocationNames:
                    return LocationNamesListModel.CreateListColumns();

                case GKListType.stEventDefs:
                    return EventDefsListModel.CreateListColumns();

                default:
                    return new ListColumns(GKListType.ltNone);
            }
        }

        public void LoadFromFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (var lt = GKListType.ltFirst; lt <= GKListType.ltLast; lt++) {
                fOptions[(int)lt].LoadFromFile(iniFile, optsVersion);
            }
        }

        public void SaveToFile(IniFile iniFile, int optsVersion)
        {
            if (iniFile == null)
                throw new ArgumentNullException("iniFile");

            for (var lt = GKListType.ltFirst; lt <= GKListType.ltLast; lt++) {
                fOptions[(int)lt].SaveToFile(iniFile, optsVersion);
            }
        }
    }
}
