/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GDModel;
using GKCore;
using GKCore.Locales;
using GKCore.Media;

namespace GKcli.Commands;

internal static class RuntimeData
{
    public static readonly Dictionary<string, GDMRecordType> RecordTypeMap = new Dictionary<string, GDMRecordType>(StringComparer.OrdinalIgnoreCase) {
        ["Individual"] = GDMRecordType.rtIndividual,
        ["Family"] = GDMRecordType.rtFamily,
        ["Note"] = GDMRecordType.rtNote,
        ["Source"] = GDMRecordType.rtSource,
        ["Repository"] = GDMRecordType.rtRepository,
        ["Multimedia"] = GDMRecordType.rtMultimedia,
        ["Group"] = GDMRecordType.rtGroup,
        ["Task"] = GDMRecordType.rtTask,
        ["Research"] = GDMRecordType.rtResearch,
        ["Communication"] = GDMRecordType.rtCommunication,
        ["Location"] = GDMRecordType.rtLocation
    };

    public static readonly Dictionary<string, GDMMediaType> MediaTypeMap;
    public static readonly Dictionary<string, MediaStoreType> StoreTypeMap;

    public static readonly Dictionary<string, GDMResearchPriority> PriorityMap;
    public static readonly Dictionary<string, GDMResearchStatus> StatusMap;

    static RuntimeData()
    {
        MediaTypeMap = new Dictionary<string, GDMMediaType>();
        for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
            MediaTypeMap.Add(LangMan.LS(GKData.MediaTypes[(int)mt]), mt);
        }

        StoreTypeMap = new Dictionary<string, MediaStoreType>();
        for (var st = MediaStoreType.mstReference; st <= MediaStoreType.mstURL; st++) {
            StoreTypeMap.Add(LangMan.LS(GKData.GKStoreTypes[(int)st].Name), st);
        }

        PriorityMap = new Dictionary<string, GDMResearchPriority>();
        for (GDMResearchPriority pt = GDMResearchPriority.rpNone; pt <= GDMResearchPriority.rpTop; pt++) {
            PriorityMap.Add(LangMan.LS(GKData.PriorityNames[(int)pt]), pt);
        }

        StatusMap = new Dictionary<string, GDMResearchStatus>();
        for (var st = GDMResearchStatus.rsDefined; st <= GDMResearchStatus.rsWithdrawn; st++) {
            StatusMap.Add(LangMan.LS(GKData.StatusNames[(int)st]), st);
        }
    }
}
