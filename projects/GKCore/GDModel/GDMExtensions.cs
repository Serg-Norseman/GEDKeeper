/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Runtime.CompilerServices;
using GDModel.Providers.GEDCOM;

namespace GDModel
{
    /// <summary>
    /// 
    /// </summary>
    public static class GDMExtensions
    {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool IsEmpty<T>(this GDMList<T> source) where T : GDMTag
        {
            return source == null || source.Count == 0;
        }

        public static GDMNotes AddNote(this IGDMStructWithNotes _struct, GDMNoteRecord noteRec)
        {
            GDMNotes note = null;

            if (noteRec != null) {
                note = new GDMNotes();
                note.XRef = noteRec.XRef;
                _struct.Notes.Add(note);
            }

            return note;
        }

        public static GDMNotes FindNotes(this IGDMStructWithNotes _struct, GDMNoteRecord noteRec)
        {
            if (noteRec != null && _struct.HasNotes) {
                int num = _struct.Notes.Count;
                for (int i = 0; i < num; i++) {
                    var notes = _struct.Notes[i];
                    if (notes.XRef == noteRec.XRef) {
                        return notes;
                    }
                }
            }
            return null;
        }

        public static GDMSourceCitation AddSource(this IGDMStructWithSourceCitations _struct, GDMSourceRecord sourceRec, string page, int quality)
        {
            GDMSourceCitation cit = null;

            if (sourceRec != null) {
                cit = new GDMSourceCitation();
                cit.XRef = sourceRec.XRef;
                cit.Page = page;
                cit.CertaintyAssessment = quality;
                _struct.SourceCitations.Add(cit);
            }

            return cit;
        }

        public static GDMSourceCitation FindSourceCitation(this IGDMStructWithSourceCitations _struct, GDMSourceRecord sourceRec)
        {
            if (sourceRec != null && _struct.HasSourceCitations) {
                int num = _struct.SourceCitations.Count;
                for (int i = 0; i < num; i++) {
                    var sourCit = _struct.SourceCitations[i];
                    if (sourCit.XRef == sourceRec.XRef) {
                        return sourCit;
                    }
                }
            }
            return null;
        }

        public static GDMMultimediaLink AddMultimedia(this IGDMStructWithMultimediaLinks _struct, GDMMultimediaRecord mediaRec)
        {
            GDMMultimediaLink result = null;

            if (mediaRec != null) {
                result = new GDMMultimediaLink();
                result.XRef = mediaRec.XRef;
                _struct.MultimediaLinks.Add(result);
            }

            return result;
        }

        public static GDMMultimediaLink FindMultimediaLink(this IGDMStructWithMultimediaLinks _struct, GDMMultimediaRecord mmRec)
        {
            if (mmRec != null && _struct.HasMultimediaLinks) {
                int num = _struct.MultimediaLinks.Count;
                for (int i = 0; i < num; i++) {
                    var link = _struct.MultimediaLinks[i];
                    if (link.XRef == mmRec.XRef) {
                        return link;
                    }
                }
            }
            return null;
        }

        public static void AddUserRef(this IGDMStructWithUserReferences _struct, string reference)
        {
            GDMUserReference uRef = new GDMUserReference();
            uRef.StringValue = reference;
            _struct.UserReferences.Add(uRef);
        }


        public static GDMIndividualRecord GetPtrValue(this GDMTree tree, GDMAssociation ptr)
        {
            return tree.GetPtrValue<GDMIndividualRecord>(ptr);
        }

        public static GDMIndividualRecord GetPtrValue(this GDMTree tree, GDMIndividualLink ptr)
        {
            return tree.GetPtrValue<GDMIndividualRecord>(ptr);
        }

        public static GDMMultimediaRecord GetPtrValue(this GDMTree tree, GDMMultimediaLink ptr)
        {
            return tree.GetPtrValue<GDMMultimediaRecord>(ptr);
        }

        public static GDMNoteRecord GetPtrValue(this GDMTree tree, GDMNotes ptr)
        {
            return tree.GetPtrValue<GDMNoteRecord>(ptr);
        }

        public static GDMFamilyRecord GetPtrValue(this GDMTree tree, GDMChildToFamilyLink ptr)
        {
            return tree.GetPtrValue<GDMFamilyRecord>(ptr);
        }

        public static GDMRepositoryRecord GetPtrValue(this GDMTree tree, GDMRepositoryCitation ptr)
        {
            return tree.GetPtrValue<GDMRepositoryRecord>(ptr);
        }

        public static GDMFamilyRecord GetPtrValue(this GDMTree tree, GDMSpouseToFamilyLink ptr)
        {
            return tree.GetPtrValue<GDMFamilyRecord>(ptr);
        }

        public static GDMSourceRecord GetPtrValue(this GDMTree tree, GDMSourceCitation ptr)
        {
            return tree.GetPtrValue<GDMSourceRecord>(ptr);
        }


        /// <summary>
        /// Attention: returns only the first marriage!
        /// </summary>
        /// <returns></returns>
        public static GDMFamilyRecord GetMarriageFamily(this GDMTree tree, GDMIndividualRecord indiRec)
        {
            GDMFamilyRecord result = (indiRec.SpouseToFamilyLinks.Count < 1) ? null : tree.GetPtrValue(indiRec.SpouseToFamilyLinks[0]);
            return result;
        }

        /// <summary>
        /// Attention: returns only the first parents family!
        /// </summary>
        /// <returns></returns>
        public static GDMFamilyRecord GetParentsFamily(this GDMTree tree, GDMIndividualRecord indiRec)
        {
            GDMFamilyRecord result = (indiRec.ChildToFamilyLinks.Count < 1) ? null : tree.GetPtrValue(indiRec.ChildToFamilyLinks[0]);
            return result;
        }

        public static void GetSpouses(this GDMTree tree, GDMFamilyRecord famRec,
                                      out GDMIndividualRecord husband, out GDMIndividualRecord wife)
        {
            if (famRec == null) {
                husband = null;
                wife = null;
            } else {
                husband = tree.GetPtrValue(famRec.Husband);
                wife = tree.GetPtrValue(famRec.Wife);
            }
        }

        public static void GetParents(this GDMTree tree, GDMIndividualRecord indiRec,
                                      out GDMIndividualRecord father, out GDMIndividualRecord mother)
        {
            if (indiRec == null) {
                father = null;
                mother = null;
                return;
            }

            GDMFamilyRecord fam = tree.GetParentsFamily(indiRec);
            tree.GetSpouses(fam, out father, out mother);
        }

        public static T Clone<T>(this T obj) where T : GDMTag, new()
        {
            T result = (T)Activator.CreateInstance(obj.GetType());
            result.Assign(obj);
            return result;
        }

        public static string GetEventKey(this GDMCustomEvent customEvent)
        {
            return (customEvent == null) ? string.Empty : customEvent.GetTagName() + ":" + customEvent.Classification;
        }
    }
}
