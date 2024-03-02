/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2018-2023 by Sergey V. Zhdanovskih.
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
using System.Collections.Generic;
using BSLib;
using GDModel;
using GKCore;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Interfaces;

namespace GKStdReports
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class RecordCardReport : ReportExporter
    {
        private readonly GDMRecord fRecord;
        private IFont fTitleFont;
        private IFont fBUFont;
        private IFont fTextFont;
        private IFont fLinkFont;
        private IFont fHeaderFont;

        public RecordCardReport(IBaseWindow baseWin, GDMRecord selectedRecord)
            : base(baseWin, false)
        {
            fTitle = SRLangMan.LS(PLS.RecordCardReport);
            fRecord = selectedRecord;
        }

        protected override void InternalGenerate()
        {
            IColor clrBlack = AppHost.GfxProvider.CreateColor(0x000000);
            IColor clrBlue = AppHost.GfxProvider.CreateColor(0x0000FF);

            fTitleFont = fWriter.CreateFont("", 14f, true, false, clrBlack);
            fHeaderFont = fWriter.CreateFont("", 12f, true, false, clrBlack);
            fBUFont = fWriter.CreateFont("", 10f, true, true, clrBlack);
            fTextFont = fWriter.CreateFont("", 10f, false, false, clrBlack);
            fLinkFont = fWriter.CreateFont("", 10f, false, true, clrBlue);

            IBaseContext baseContext = fBase.Context;

            AddParagraph(fRecord.XRef, fTextFont, TextAlignment.taRight);

            switch (fRecord.RecordType) {
                case GDMRecordType.rtIndividual:
                    GenIndividualCard(baseContext, fRecord as GDMIndividualRecord);
                    break;

                case GDMRecordType.rtFamily:
                    GenFamilyCard(fRecord as GDMFamilyRecord);
                    break;

                case GDMRecordType.rtNote:
                    GenNoteCard(fRecord as GDMNoteRecord);
                    break;

                case GDMRecordType.rtMultimedia:
                    GenMultimediaCard(fRecord as GDMMultimediaRecord);
                    break;

                case GDMRecordType.rtSource:
                    GenSourceCard(fRecord as GDMSourceRecord);
                    break;

                case GDMRecordType.rtRepository:
                    GenRepositoryCard(fRecord as GDMRepositoryRecord);
                    break;

                case GDMRecordType.rtGroup:
                    GenGroupCard(fRecord as GDMGroupRecord);
                    break;

                case GDMRecordType.rtResearch:
                    GenResearchCard(fRecord as GDMResearchRecord);
                    break;

                case GDMRecordType.rtTask:
                    GenTaskCard(fRecord as GDMTaskRecord);
                    break;

                case GDMRecordType.rtCommunication:
                    GenCommunicationCard(fRecord as GDMCommunicationRecord);
                    break;

                case GDMRecordType.rtLocation:
                    GenLocationCard(fRecord as GDMLocationRecord);
                    break;
            }
        }

        private void GenIndividualCard(IBaseContext baseContext, GDMIndividualRecord iRec)
        {
            if (iRec == null) return;

            try {
                GDMTree tree = baseContext.Tree;

                fWriter.NewLine();

                bool firstSurname = fOptions.SurnameFirstInOrder;
                for (int i = 0; i < iRec.PersonalNames.Count; i++) {
                    var persName = iRec.PersonalNames[i];
                    fWriter.AddParagraph(GKUtils.GetNameString(iRec, persName, firstSurname, true), fBUFont);
                }

                fWriter.AddParagraph(LangMan.LS(LSID.Sex) + ": " + GKUtils.SexStr(iRec.Sex), fTextFont);

                ShowParentsInfo(iRec);
                ShowSpousesInfo(baseContext, iRec);

                RecListIndividualEventsRefresh(iRec);
                RecListNotesRefresh(iRec);
                RecListMediaRefresh(iRec);
                RecListSourcesRefresh(iRec);
                RecListAssociationsRefresh(iRec);
                RecListGroupsRefresh(iRec);

                //ShowPersonNamesakes(tree, iRec, summary);
                //ShowPersonExtInfo(tree, iRec, summary);
                //ShowRFN(iRec, summary);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenIndividualCard()", ex);
            }
        }

        private void ShowParentsInfo(GDMIndividualRecord iRec)
        {
            try {
                for (int p = 0; p < iRec.ChildToFamilyLinks.Count; p++) {
                    var ctfLink = iRec.ChildToFamilyLinks[p];
                    var famRec = fTree.GetPtrValue(ctfLink);

                    GDMIndividualRecord father, mother;
                    fTree.GetSpouses(famRec, out father, out mother);

                    if (father != null || mother != null) {
                        var plType = ctfLink.PedigreeLinkageType;
                        string linkType =
                            (plType == GDMPedigreeLinkageType.plNone || plType == GDMPedigreeLinkageType.plBirth) ?
                            string.Empty : string.Format(" ({0})", LangMan.LS(GKData.ParentTypes[(int)plType]));

                        fWriter.NewLine();
                        fWriter.AddParagraph(LangMan.LS(LSID.Parents) + linkType + ":", fTextFont);

                        string st;

                        st = (father == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(father.XRef, GKUtils.GetNameString(father, false));
                        fWriter.AddParagraph("  " + LangMan.LS(LSID.Father) + ": " + st + GKUtils.GetLifeStr(father), fTextFont);

                        st = (mother == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(mother.XRef, GKUtils.GetNameString(mother, false));
                        fWriter.AddParagraph("  " + LangMan.LS(LSID.Mother) + ": " + st + GKUtils.GetLifeStr(mother), fTextFont);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.ShowParentsInfo()", ex);
            }
        }

        private void ShowSpousesInfo(IBaseContext baseContext, GDMIndividualRecord iRec)
        {
            try {
                for (int i = 0; i < iRec.SpouseToFamilyLinks.Count; i++) {
                    GDMFamilyRecord family = fTree.GetPtrValue(iRec.SpouseToFamilyLinks[i]);
                    if (family == null) continue;
                    if (!baseContext.IsRecordAccess(family.Restriction)) continue;

                    string st;
                    GDMIndividualRecord spRec;
                    string unk;
                    if (iRec.Sex == GDMSex.svMale) {
                        spRec = fTree.GetPtrValue(family.Wife);
                        st = LangMan.LS(LSID.Wife) + ": ";
                        unk = LangMan.LS(LSID.UnkFemale);
                    } else {
                        spRec = fTree.GetPtrValue(family.Husband);
                        st = LangMan.LS(LSID.Husband) + ": ";
                        unk = LangMan.LS(LSID.UnkMale);
                    }
                    string marr = GKUtils.GetMarriageDateStr(family, fOptions.DefDateFormat);
                    if (marr != "") {
                        marr = LangMan.LS(LSID.LMarriage) + " " + marr;
                    } else {
                        marr = LangMan.LS(LSID.LFamily);
                    }

                    fWriter.NewLine();
                    if (spRec != null) {
                        st = st + HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)) + " (" + HyperLink(family.XRef, marr) + ")";
                    } else {
                        st = st + unk + " (" + HyperLink(family.XRef, marr) + ")";
                    }
                    fWriter.AddParagraph(st, fTextFont);

                    int chNum = family.Children.Count;
                    if (chNum != 0) {
                        fWriter.NewLine();
                        fWriter.AddParagraph(LangMan.LS(LSID.Childs) + ":", fTextFont);

                        for (int k = 0; k < chNum; k++) {
                            GDMIndividualRecord child = fTree.GetPtrValue(family.Children[k]);
                            if (child == null) continue;

                            fWriter.AddParagraph("    " + HyperLink(child.XRef, GKUtils.GetNameString(child, false)) + GKUtils.GetLifeStr(child), fTextFont);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.ShowSpousesInfo()", ex);
            }
        }

        private void RecListIndividualEventsRefresh(GDMIndividualRecord record)
        {
            try {
                if (record.HasEvents) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.Events) + ":", fTextFont);

                    for (int i = 0; i < record.Events.Count; i++) {
                        fWriter.NewLine();

                        GDMCustomEvent evt = record.Events[i];
                        string st = GKUtils.GetEventName(evt);

                        string sv = GetFactValueStr(evt);
                        if (!string.IsNullOrEmpty(sv)) {
                            sv += ", ";
                        }
                        fWriter.AddParagraph("  " + st + ": " + sv + GKUtils.GetEventDesc(fTree, evt, false), fTextFont);

                        ShowDetailCause(evt);
                        if (evt.HasAddress) {
                            ShowAddressSummary(evt.Address);
                        }
                        RecListSourcesRefresh(evt, 6.0f);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListIndividualEventsRefresh()", ex);
            }
        }

        private void RecListMediaRefresh(GDMRecord record)
        {
            try {
                if (record.HasMultimediaLinks) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPMultimedia) + " (" + record.MultimediaLinks.Count.ToString() + "):", fTextFont);

                    for (int i = 0; i < record.MultimediaLinks.Count; i++) {
                        GDMMultimediaLink mmLink = record.MultimediaLinks[i];
                        GDMMultimediaRecord mmRec = fTree.GetPtrValue<GDMMultimediaRecord>(mmLink);
                        if (mmRec == null || mmRec.FileReferences.Count == 0) continue;

                        string st = mmRec.FileReferences[0].Title;
                        AddParagraph("  " + HyperLink(mmRec.XRef, st) + " (" +
                                    HyperLink(GKData.INFO_HREF_VIEW + mmRec.XRef, LangMan.LS(LSID.MediaView)) + ")", fTextFont, TextAlignment.taLeft, 6.0f);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListMediaRefresh()", ex);
            }
        }

        private void RecListNotesRefresh(GDMRecord record)
        {
            try {
                if (record.HasNotes) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPNotes) + " (" + record.Notes.Count.ToString() + "):", fTextFont);

                    for (int i = 0; i < record.Notes.Count; i++) {
                        if (i > 0) {
                            fWriter.NewLine();
                        }

                        GDMLines noteLines = fTree.GetNoteLines(record.Notes[i]);
                        for (int k = 0; k < noteLines.Count; k++) {
                            AddParagraph(noteLines[k], fTextFont, TextAlignment.taLeft, 6.0f);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListNotesRefresh()", ex);
            }
        }

        private void RecListSourcesRefresh(IGDMStructWithSourceCitations structWSC, float indent = 0.0f)
        {
            if (structWSC == null) return;

            try {
                if (structWSC.HasSourceCitations) {
                    if (structWSC is IGDMRecord) {
                        fWriter.NewLine();
                    }

                    AddParagraph(LangMan.LS(LSID.RPSources) + " (" + structWSC.SourceCitations.Count.ToString() + "):", fTextFont, TextAlignment.taLeft, indent);

                    for (int i = 0; i < structWSC.SourceCitations.Count; i++) {
                        GDMSourceCitation sourCit = structWSC.SourceCitations[i];
                        GDMSourceRecord sourceRec = fTree.GetPtrValue<GDMSourceRecord>(sourCit);
                        if (sourceRec == null) continue;

                        string nm = "\"" + sourceRec.ShortTitle + "\"";
                        if (!string.IsNullOrEmpty(sourCit.Page)) {
                            nm = nm + ", " + sourCit.Page;
                        }
                        AddParagraph(HyperLink(sourceRec.XRef, nm), fTextFont, TextAlignment.taLeft, indent + 6.0f);

                        var text = sourCit.Data.Text;
                        if (!text.IsEmpty()) {
                            AddParagraph(text.Lines.Text, fTextFont, TextAlignment.taLeft, indent + 12.0f);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListSourcesRefresh()", ex);
            }
        }

        private void RecListAssociationsRefresh(GDMIndividualRecord record)
        {
            try {
                if (record.HasAssociations) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.Associations) + ":", fTextFont);

                    for (int i = 0; i < record.Associations.Count; i++) {
                        GDMAssociation ast = record.Associations[i];
                        var relIndi = fTree.GetPtrValue(ast);

                        string nm = ((relIndi == null) ? string.Empty : GKUtils.GetNameString(relIndi, false));
                        string xref = ((relIndi == null) ? string.Empty : relIndi.XRef);

                        fWriter.AddParagraph("    " + ast.Relation + " " + HyperLink(xref, nm), fTextFont);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListAssociationsRefresh()", ex);
            }
        }

        private void RecListGroupsRefresh(GDMIndividualRecord record)
        {
            try {
                if (record.HasGroups) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPGroups) + ":", fTextFont);

                    for (int i = 0; i < record.Groups.Count; i++) {
                        GDMPointer ptr = record.Groups[i];
                        GDMGroupRecord grp = fTree.GetPtrValue<GDMGroupRecord>(ptr);
                        if (grp == null) continue;

                        fWriter.AddParagraph("    " + HyperLink(grp.XRef, grp.GroupName), fTextFont);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListGroupsRefresh()", ex);
            }
        }

        private void ShowDetailCause(GDMCustomEvent evt)
        {
            string cause = GKUtils.GetEventCause(evt);
            if (!string.IsNullOrEmpty(cause)) {
                fWriter.AddParagraph("    " + cause, fTextFont);
            }
        }

        private void ShowAddressSummary(GDMAddress address)
        {
            if (address != null && !address.IsEmpty()) {
                fWriter.AddParagraph("    " + LangMan.LS(LSID.Address) + ":", fTextFont);

                string ts = "";
                if (address.AddressCountry != "") {
                    ts = ts + address.AddressCountry + ", ";
                }
                if (address.AddressState != "") {
                    ts = ts + address.AddressState + ", ";
                }
                if (address.AddressCity != "") {
                    ts += address.AddressCity;
                }
                if (ts != "") {
                    fWriter.AddParagraph("    " + ts, fTextFont);
                }

                ts = "";
                if (address.AddressPostalCode != "") {
                    ts = ts + address.AddressPostalCode + ", ";
                }
                if (address.Lines.Text.Trim() != "") {
                    ts += address.Lines.Text.Trim();
                }
                if (ts != "") {
                    fWriter.AddParagraph("    " + ts, fTextFont);
                }

                for (int i = 0; i < address.PhoneNumbers.Count; i++) {
                    fWriter.AddParagraph("    " + address.PhoneNumbers[i].StringValue, fTextFont);
                }

                for (int i = 0; i < address.EmailAddresses.Count; i++) {
                    fWriter.AddParagraph("    " + address.EmailAddresses[i].StringValue, fTextFont);
                }

                for (int i = 0; i < address.WebPages.Count; i++) {
                    fWriter.AddParagraph("    " + address.WebPages[i].StringValue, fTextFont);
                }
            }
        }

        private static string GetFactValueStr(GDMCustomEvent evt)
        {
            string result = evt.StringValue;
            if (result.StartsWith(GKData.INFO_HTTP_PREFIX)) {
                result = HyperLink(result, result);
            }
            return result;
        }

        private void GenFamilyCard(GDMFamilyRecord familyRec)
        {
            if (familyRec == null) return;

            try {
                fWriter.NewLine();

                GDMIndividualRecord spRec = fTree.GetPtrValue(familyRec.Husband);
                string st = ((spRec == null) ? LangMan.LS(LSID.UnkMale) : HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)));
                fWriter.AddParagraph(LangMan.LS(LSID.Husband) + ": " + st + GKUtils.GetLifeStr(spRec), fTextFont);

                spRec = fTree.GetPtrValue(familyRec.Wife);
                st = ((spRec == null) ? LangMan.LS(LSID.UnkFemale) : HyperLink(spRec.XRef, GKUtils.GetNameString(spRec, false)));
                fWriter.AddParagraph(LangMan.LS(LSID.Wife) + ": " + st + GKUtils.GetLifeStr(spRec), fTextFont);

                fWriter.NewLine();
                if (familyRec.Children.Count != 0) {
                    fWriter.AddParagraph(LangMan.LS(LSID.Childs) + ":", fTextFont);

                    for (int i = 0; i < familyRec.Children.Count; i++) {
                        var child = fTree.GetPtrValue(familyRec.Children[i]);
                        AddParagraph(HyperLink(child.XRef, GKUtils.GetNameString(child, false)) + GKUtils.GetLifeStr(child), fTextFont, TextAlignment.taLeft, 6.0f);
                    }

                    fWriter.NewLine();
                }

                RecListFamilyEventsRefresh(familyRec);
                RecListNotesRefresh(familyRec);
                RecListMediaRefresh(familyRec);
                RecListSourcesRefresh(familyRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenFamilyCard()", ex);
            }
        }

        private void RecListFamilyEventsRefresh(GDMFamilyRecord record)
        {
            try {
                if (record.HasEvents) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.Events) + ":", fTextFont);

                    for (int i = 0; i < record.Events.Count; i++) {
                        fWriter.NewLine();

                        GDMFamilyEvent evt = (GDMFamilyEvent)record.Events[i];

                        string st = GKUtils.GetEventName(evt);
                        fWriter.AddParagraph("  " + st + ": " + GKUtils.GetEventDesc(fTree, evt, false), fTextFont);

                        ShowDetailCause(evt);
                        RecListSourcesRefresh(evt, 6.0f);
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.RecListFamilyEventsRefresh()", ex);
            }
        }

        private void GenNoteCard(GDMNoteRecord noteRec)
        {
            if (noteRec == null) return;

            try {
                fWriter.NewLine();
                fWriter.AddParagraph(noteRec.Lines.Text, fTextFont, TextAlignment.taJustify);

                ShowSubjectLinks(noteRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenNoteCard()", ex);
            }
        }

        private void GenGroupCard(GDMGroupRecord groupRec)
        {
            try {
                StringList mbrList = new StringList();
                try {
                    fWriter.NewLine();
                    fWriter.AddParagraph(groupRec.GroupName, fBUFont);
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.Members) + " (" + groupRec.Members.Count.ToString() + "):", fTextFont);

                    int num = groupRec.Members.Count;
                    for (int i = 0; i < num; i++) {
                        GDMPointer ptr = groupRec.Members[i];
                        var member = fTree.GetPtrValue<GDMIndividualRecord>(ptr);

                        mbrList.AddObject(GKUtils.GetNameString(member, false), member);
                    }

                    mbrList.Sort();
                    for (int i = 0; i < mbrList.Count; i++) {
                        GDMIndividualRecord member = (GDMIndividualRecord)mbrList.GetObject(i);

                        fWriter.AddParagraph("    " + HyperLink(member.XRef, mbrList[i]), fTextFont);
                    }

                    RecListNotesRefresh(groupRec);
                    RecListMediaRefresh(groupRec);
                } finally {
                    mbrList.Dispose();
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenGroupCard()", ex);
            }
        }

        private void GenMultimediaCard(GDMMultimediaRecord mediaRec)
        {
            try {
                GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
                string mediaTitle = (fileRef == null) ? LangMan.LS(LSID.Unknown) : fileRef.Title;

                fWriter.NewLine();
                fWriter.AddParagraph(mediaTitle, fBUFont);
                fWriter.NewLine();
                if (fileRef != null) {
                    // TODO: embedded image?
                }

                ShowSubjectLinks(mediaRec);

                RecListNotesRefresh(mediaRec);
                RecListSourcesRefresh(mediaRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenMultimediaCard()", ex);
            }
        }

        private void GenSourceCard(GDMSourceRecord sourceRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(sourceRec.ShortTitle, fBUFont);
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Author) + ": " + sourceRec.Originator.Lines.Text.Trim(), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Title) + ": \"" + sourceRec.Title.Lines.Text.Trim() + "\"", fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Publication) + ": \"" + sourceRec.Publication.Lines.Text.Trim() + "\"", fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Text) + ": \"" + sourceRec.Text.Lines.Text.Trim() + "\"", fTextFont);

                if (sourceRec.RepositoryCitations.Count > 0) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPRepositories) + ":", fTextFont);

                    for (int i = 0; i < sourceRec.RepositoryCitations.Count; i++) {
                        GDMRepositoryRecord rep = fTree.GetPtrValue<GDMRepositoryRecord>(sourceRec.RepositoryCitations[i]);

                        fWriter.AddParagraph("    " + HyperLink(rep.XRef, rep.RepositoryName), fTextFont);
                    }
                }

                ShowSubjectLinks(sourceRec);

                RecListNotesRefresh(sourceRec);
                RecListMediaRefresh(sourceRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenSourceCard()", ex);
            }
        }

        private void GenRepositoryCard(GDMRepositoryRecord repositoryRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(repositoryRec.RepositoryName.Trim(), fBUFont);
                fWriter.NewLine();

                if (repositoryRec.HasAddress)
                    ShowAddressSummary(repositoryRec.Address);

                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.RPSources) + ":", fTextFont);

                var sortedSources = new List<Tuple<string, string>>();
                int num = fTree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = fTree[i];

                    if (rec.RecordType == GDMRecordType.rtSource) {
                        GDMSourceRecord srcRec = (GDMSourceRecord)rec;

                        for (int j = 0; j < srcRec.RepositoryCitations.Count; j++) {
                            if (srcRec.RepositoryCitations[j].XRef == repositoryRec.XRef) {
                                sortedSources.Add(GenRecordLinkTuple(srcRec, false));
                            }
                        }
                    }
                }
                sortedSources.Sort();
                foreach (var tpl in sortedSources) {
                    fWriter.AddParagraph("    " + tpl.Item2, fTextFont);
                }

                RecListNotesRefresh(repositoryRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenRepositoryCard()", ex);
            }
        }

        private void GenResearchCard(GDMResearchRecord researchRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Title) + ": \"" + researchRec.ResearchName.Trim() + "\"", fBUFont);
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)researchRec.Priority]), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Status) + ": " + LangMan.LS(GKData.StatusNames[(int)researchRec.Status]) + " (" + researchRec.Percent.ToString() + "%)", fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.StartDate) + ": " + researchRec.StartDate.GetDisplayString(fOptions.DefDateFormat), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.StopDate) + ": " + researchRec.StopDate.GetDisplayString(fOptions.DefDateFormat), fTextFont);

                if (researchRec.Tasks.Count > 0) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPTasks) + ":", fTextFont);

                    for (int i = 0; i < researchRec.Tasks.Count; i++) {
                        var taskRec = fTree.GetPtrValue<GDMTaskRecord>(researchRec.Tasks[i]);
                        fWriter.AddParagraph("    " + GenRecordLink(taskRec, false), fTextFont);
                    }
                }

                if (researchRec.Communications.Count > 0) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPCommunications) + ":", fTextFont);

                    for (int i = 0; i < researchRec.Communications.Count; i++) {
                        var corrRec = fTree.GetPtrValue<GDMCommunicationRecord>(researchRec.Communications[i]);
                        fWriter.AddParagraph("    " + GenRecordLink(corrRec, false), fTextFont);
                    }
                }

                if (researchRec.Groups.Count != 0) {
                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.RPGroups) + ":", fTextFont);

                    for (int i = 0; i < researchRec.Groups.Count; i++) {
                        var grp = fTree.GetPtrValue<GDMGroupRecord>(researchRec.Groups[i]);
                        fWriter.AddParagraph("    " + HyperLink(grp.XRef, grp.GroupName), fTextFont);
                    }
                }

                RecListNotesRefresh(researchRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenResearchCard()", ex);
            }
        }

        private void GenTaskCard(GDMTaskRecord taskRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Goal) + ": " + GKUtils.GetTaskGoalStr(fTree, taskRec) + "", fBUFont);
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Priority) + ": " + LangMan.LS(GKData.PriorityNames[(int)taskRec.Priority]), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.StartDate) + ": " + taskRec.StartDate.GetDisplayString(fOptions.DefDateFormat), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.StopDate) + ": " + taskRec.StopDate.GetDisplayString(fOptions.DefDateFormat), fTextFont);

                RecListNotesRefresh(taskRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenTaskCard()", ex);
            }
        }

        private void GenCommunicationCard(GDMCommunicationRecord commRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Theme) + ": \"" + commRec.CommName.Trim() + "\"", fBUFont);
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Corresponder) + ": " + GKUtils.GetCorresponderStr(fTree, commRec, true), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Type) + ": " + LangMan.LS(GKData.CommunicationNames[(int)commRec.CommunicationType]), fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Date) + ": " + commRec.Date.GetDisplayString(fOptions.DefDateFormat), fTextFont);

                RecListNotesRefresh(commRec);
                RecListMediaRefresh(commRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenCommunicationCard()", ex);
            }
        }

        private void GenLocationCard(GDMLocationRecord locRec)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(locRec.LocationName.Trim(), fBUFont);
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Latitude) + ": " + locRec.Map.Lati, fTextFont);
                fWriter.AddParagraph(LangMan.LS(LSID.Longitude) + ": " + locRec.Map.Long, fTextFont);

                var linkList = GKUtils.GetLocationLinks(fTree, locRec);
                if (linkList.Count > 0) {
                    linkList.Sort();

                    fWriter.NewLine();
                    fWriter.AddParagraph(LangMan.LS(LSID.Links) + ":", fTextFont);

                    for (int i = 0; i < linkList.Count; i++) {
                        GDMRecord rec = linkList.GetObject(i) as GDMRecord;
                        AddParagraph("    " + HyperLink(rec.XRef, linkList[i]), fTextFont, TextAlignment.taLeft, 6.0f);
                    }
                }

                RecListNotesRefresh(locRec);
                RecListMediaRefresh(locRec);
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.GenLocationCard()", ex);
            }
        }

        private void ShowSubjectLinks(GDMRecord subject, float indent = 6.0f)
        {
            try {
                fWriter.NewLine();
                fWriter.AddParagraph(LangMan.LS(LSID.Links) + ":", fTextFont);

                for (int m = 0; m < fTree.RecordsCount; m++) {
                    var record = fTree[m];

                    if (subject is GDMNoteRecord && record.HasNotes) {
                        for (int i = 0; i < record.Notes.Count; i++) {
                            if (record.Notes[i].XRef == subject.XRef) {
                                ShowLink(subject, record, null, null, indent);
                            }
                        }
                    } else if (subject is GDMMultimediaRecord && record.HasMultimediaLinks) {
                        for (int i = 0; i < record.MultimediaLinks.Count; i++) {
                            if (record.MultimediaLinks[i].XRef == subject.XRef) {
                                ShowLink(subject, record, null, null, indent);
                            }
                        }
                    } else if (subject is GDMSourceRecord && record.HasSourceCitations) {
                        for (int i = 0; i < record.SourceCitations.Count; i++) {
                            var sourCit = record.SourceCitations[i];
                            if (sourCit.XRef == subject.XRef) {
                                ShowLink(subject, record, null, sourCit, indent);
                            }
                        }
                    }

                    var evsRec = record as GDMRecordWithEvents;
                    if (evsRec != null && evsRec.HasEvents) {
                        for (int i = 0; i < evsRec.Events.Count; i++) {
                            ShowEvent(subject, evsRec, evsRec.Events[i], indent);
                        }
                    }
                }
            } catch (Exception ex) {
                Logger.WriteError("RecordCardReport.ShowSubjectLinks()", ex);
            }
        }

        private void ShowEvent(GDMRecord subj, GDMRecord aRec, GDMCustomEvent evt, float indent = 0.0f)
        {
            switch (subj.RecordType) {
                case GDMRecordType.rtNote:
                    if (evt.HasNotes) {
                        for (int i = 0; i < evt.Notes.Count; i++) {
                            if (evt.Notes[i].XRef == subj.XRef) {
                                ShowLink(subj, aRec, evt, null, indent);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtMultimedia:
                    if (evt.HasMultimediaLinks) {
                        for (int i = 0; i < evt.MultimediaLinks.Count; i++) {
                            if (evt.MultimediaLinks[i].XRef == subj.XRef) {
                                ShowLink(subj, aRec, evt, null, indent);
                            }
                        }
                    }
                    break;

                case GDMRecordType.rtSource:
                    if (evt.HasSourceCitations) {
                        for (int i = 0; i < evt.SourceCitations.Count; i++) {
                            if (evt.SourceCitations[i].XRef == subj.XRef) {
                                ShowLink(subj, aRec, evt, evt.SourceCitations[i], indent);
                            }
                        }
                    }
                    break;
            }
        }

        private void ShowLink(GDMRecord aSubject, GDMRecord aRec, GDMTag aTag, GDMPointer aExt, float indent = 0.0f)
        {
            string prefix;
            if (aSubject is GDMSourceRecord && aExt != null) {
                GDMSourceCitation cit = (aExt as GDMSourceCitation);
                if (cit != null && !string.IsNullOrEmpty(cit.Page)) {
                    prefix = cit.Page + ": ";
                } else {
                    prefix = "";
                }
            } else {
                prefix = "";
            }

            string suffix;
            if (aTag is GDMCustomEvent) {
                suffix = ", " + GKUtils.GetEventNameLd((GDMCustomEvent)aTag);
            } else {
                suffix = "";
            }

            AddParagraph(prefix + GenRecordLink(aRec, true) + suffix, fTextFont, TextAlignment.taLeft, indent);
        }

        private void AddParagraph(string text, IFont font, TextAlignment alignment = TextAlignment.taLeft, float indent = 0.0f)
        {
            fWriter.BeginParagraph(alignment, 0, 0, indent);
            fWriter.AddParagraphChunk(text, font);
            fWriter.EndParagraph();
        }

        private Tuple<string, string> GenRecordLinkTuple(GDMRecord record, bool signed)
        {
            if (record != null) {
                string recName = GKUtils.GetRecordName(fTree, record, signed);
                string recLink = HyperLink(record.XRef, recName);
                return new Tuple<string, string>(recName, recLink);
            } else {
                return new Tuple<string, string>(string.Empty, string.Empty);
            }
        }

        private string GenRecordLink(GDMRecord record, bool signed)
        {
            string result = "";

            if (record != null) {
                result = HyperLink(record.XRef, GKUtils.GetRecordName(fTree, record, signed));
            }

            return result;
        }

        private static string HyperLink(string xref, string text)
        {
            string result = "";

            if (!string.IsNullOrEmpty(xref) && string.IsNullOrEmpty(text)) {
                text = "???";
            }

            /*if (!string.IsNullOrEmpty(xref) && !string.IsNullOrEmpty(text)) {
                result = "[url=" + xref + "]" + text + "[/url]";
            }*/
            result = text;

            return result;
        }
    }
}
