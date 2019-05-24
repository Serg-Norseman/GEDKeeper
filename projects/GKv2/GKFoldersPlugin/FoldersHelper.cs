/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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
using GDModel;
using GDModel.Providers.GEDCOM;

namespace GKFoldersPlugin
{
    /// <summary>
    /// 
    /// </summary>
    public static class FoldersHelper
    {
        public static string GetFolder(GDMRecord record)
        {
            var folderTag = record.FindTag(GEDCOMTagType._FOLDER, 0);
            return (folderTag == null) ? "" : folderTag.StringValue;
        }

        public static void SetFolder(GDMRecord record, string value)
        {
            if (!HasFolderSupport(record.RecordType)) {
                return;
            }

            var folderTag = record.FindTag(GEDCOMTagType._FOLDER, 0);
            if (!string.IsNullOrEmpty(value)) {
                if (folderTag == null) {
                    record.AddTag(new GDMTag(record, GEDCOMTagType._FOLDER, value));
                } else {
                    folderTag.StringValue = value;
                }
            } else {
                if (folderTag != null) {
                    record.DeleteTag(GEDCOMTagType._FOLDER);
                }
            }
        }

        public static bool HasFolderSupport(GDMRecordType recType)
        {
            bool result = false;

            switch (recType) {
                case GDMRecordType.rtNone:
                case GDMRecordType.rtNote:
                case GDMRecordType.rtMultimedia:
                case GDMRecordType.rtSource:
                case GDMRecordType.rtRepository:
                case GDMRecordType.rtLocation:
                case GDMRecordType.rtSubmission:
                case GDMRecordType.rtSubmitter:
                    break;

                case GDMRecordType.rtIndividual:
                case GDMRecordType.rtFamily:
                case GDMRecordType.rtGroup:
                case GDMRecordType.rtResearch:
                case GDMRecordType.rtTask:
                case GDMRecordType.rtCommunication:
                    result = true;
                    break;
            }

            return result;
        }
    }
}
