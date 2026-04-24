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
using GKCore.Events;
using GKCore.Locales;
using GKCore.Media;

namespace GKcli.Commands;

internal static class RuntimeData
{
    // "Date string, strictly with the GEDCOM Date Spec" - lax compliance, models make mistakes
    public const string GEDCOMDateFormatDirective = "Strict 'GEDCOM Date Spec' format (from 'gedcom_date_spec' tool). ISO and other formats are prohibited.";


    public static readonly Dictionary<string, EventTarget> RWETypeMap = new Dictionary<string, EventTarget>(StringComparer.OrdinalIgnoreCase) {
        ["Individual"] = EventTarget.etIndividual,
        ["Family"] = EventTarget.etFamily,
    };

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

    public static readonly Dictionary<string, GDMCommunicationType> CommTypeMap;
    public static readonly Dictionary<string, GDMCommunicationDir> CommDirMap;

    public static readonly Dictionary<string, GDMNameType> NameTypeMap;
    public static readonly Dictionary<string, GDMLanguageID> LangMap;

    public static readonly Dictionary<string, GDMPedigreeLinkageType> LinkageTypeMap;

    public static readonly Dictionary<string, GDMRestriction> RestrictionMap;

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

        CommTypeMap = new Dictionary<string, GDMCommunicationType>();
        for (GDMCommunicationType ct = GDMCommunicationType.ctCall; ct <= GDMCommunicationType.ctLast; ct++) {
            CommTypeMap.Add(LangMan.LS(GKData.CommunicationNames[(int)ct]), ct);
        }

        CommDirMap = new Dictionary<string, GDMCommunicationDir>();
        CommDirMap.Add(LangMan.LS(LSID.CD_1), GDMCommunicationDir.cdFrom);
        CommDirMap.Add(LangMan.LS(LSID.CD_2), GDMCommunicationDir.cdTo);

        NameTypeMap = new Dictionary<string, GDMNameType>();
        for (GDMNameType nt = GDMNameType.ntNone; nt <= GDMNameType.ntMarried; nt++) {
            NameTypeMap.Add(LangMan.LS(GKData.NameTypes[(int)nt]), nt);
        }

        LangMap = new Dictionary<string, GDMLanguageID>();
        for (GDMLanguageID lng = GDMLanguageID.Afrikaans; lng <= GDMLanguageID.Yiddish; lng++) {
            LangMap.Add(lng.ToString(), lng);
        }

        LinkageTypeMap = new Dictionary<string, GDMPedigreeLinkageType>();
        for (var plt = GDMPedigreeLinkageType.plNone; plt <= GDMPedigreeLinkageType.plFoster; plt++) {
            LinkageTypeMap.Add(LangMan.LS(GKData.ParentTypes[(int)plt]), plt);
        }

        RestrictionMap = new Dictionary<string, GDMRestriction>();
        for (var rs = GDMRestriction.rnNone; rs <= GDMRestriction.rnPrivacy; rs++) {
            RestrictionMap.Add(LangMan.LS(GKData.Restrictions[(int)rs]), rs);
        }
    }
}
