/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKcli.Platform;
using GKCore;
using GKCore.Media;
using GKUI.Platform;

namespace GKcli.Features;

internal class MediaGetTool : BaseTool
{
    public MediaGetTool() : base("multimedia_get") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Get a multimedia record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'O1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"❌ Multimedia record not found with XRef: {xref}");

        GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
        if (fileRef == null)
            return MCPContent.CreateSimpleContent($"❌ Media record {xref} contains no files");

        if (!GKUtils.UseEmbeddedViewer(fileRef.GetMultimediaFormat())) {
            return MCPContent.CreateSimpleContent($"❌ Media record {xref} could not be transferred");
        } else {
            //fView.SetTitle(fFileReference.Title);

            MultimediaKind mmKind = GKUtils.GetMultimediaKind(fileRef.GetMultimediaFormat());

            try {
                switch (mmKind) {
                    case MultimediaKind.mkImage: {
                            //int fileNum = mediaRec.FileReferences.IndexOf(fileRef);
                            //IImage img = baseContext.LoadMediaImage(mediaRec, fileNum, -1, -1, ExtRect.Empty, false, false);
                            Stream fs = baseContext.MediaLoad(fileRef, false);
                            if (fs != null) {
                                try {
                                    string mimeType = fileRef.MultimediaFormat;
                                    if (mimeType == "jpg") mimeType = "jpeg";

                                    return MCPHelper.CreateImageContent(fs, mimeType, [Role.User, Role.Assistant], 0.5f);
                                } finally {
                                    fs.Dispose();
                                }
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        return MCPContent.CreateSimpleContent($"❌ Media record {xref} could not be transferred (audio/video)");

                    case MultimediaKind.mkText: {
                            Stream fs = baseContext.MediaLoad(fileRef, false);
                            if (fs != null) {
                                try {
                                    string text = null;
                                    switch (fileRef.GetMultimediaFormat()) {
                                        case GDMMultimediaFormat.mfTXT:
                                            using (StreamReader strd = GKUtils.GetDetectedStreamReader(fs)) {
                                                text = strd.ReadToEnd();
                                            }
                                            break;

                                        case GDMMultimediaFormat.mfRTF:
                                            using (StreamReader strd = new StreamReader(fs)) {
                                                text = CLIHelper.StripRtf(strd.ReadToEnd());
                                            }
                                            break;

                                        case GDMMultimediaFormat.mfHTM:
                                            using (StreamReader strd = new StreamReader(fs)) {
                                                text = CLIHelper.GetPlainText(strd);
                                            }
                                            break;
                                    }
                                    if (!string.IsNullOrEmpty(text)) {
                                        return MCPContent.CreateSimpleContent(text, [Role.User, Role.Assistant], 0.5f);
                                    } else {
                                        return MCPContent.CreateSimpleContent($"❌ Media record {xref} could not be transferred (unsupported text format)");
                                    }
                                } finally {
                                    fs.Dispose();
                                }
                            }
                            break;
                        }
                }
            } catch (Exception ex) {
                Logger.WriteError("MediaGetTool.ExecuteTool().Transfer", ex);
                return MCPContent.CreateSimpleContent($"Media record {xref} caused an error");
            }
        }

        return MCPContent.CreateSimpleContent($"Multimedia record received: {xref}");
    }
}


internal class MediaUpsertTool : BaseTool
{
    public MediaUpsertTool() : base("multimedia_upsert") { }

    public override MCPTool CreateTool()
    {
        var mediaTypes = RuntimeData.MediaTypeMap.Keys.ToList();
        var storeTypes = RuntimeData.StoreTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add new multimedia record or update existing. Provide 'xref' to edit; omit 'xref' to create. 'title', 'file_path', 'media_type' and 'store_type' required for new records.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing multimedia to update (omit for new)" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the multimedia item" },
                    ["file_path"] = new MCPToolProperty { Type = "string", Description = "Path to the multimedia file on disk or URL (required for new)" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file.", Enum = mediaTypes },
                    ["store_type"] = new MCPToolProperty { Type = "string", Description = "Storage type for the multimedia file.", Enum = storeTypes }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string title = MCPHelper.GetOptionalStr(args, "title", null);
        string filePath = MCPHelper.GetOptionalStr(args, "file_path", null);
        string mediaTypeStr = MCPHelper.GetOptionalStr(args, "media_type", null);
        string storeTypeStr = MCPHelper.GetOptionalStr(args, "store_type", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
            if (mediaRec == null)
                return MCPContent.CreateSimpleContent($"❌ Multimedia record not found: '{xref}'.");

            var fileRef = mediaRec.FileReferences[0];

            if (title != null) {
                fileRef.Title = title;
            }

            if (mediaTypeStr != null) {
                if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                    return MCPContent.CreateSimpleContent($"❌ Invalid media type: '{mediaTypeStr}'.");

                fileRef.MediaType = mediaType;
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Multimedia record updated: {xref} - \"{fileRef.Title}\" ({mediaTypeStr})");
        } else {
            if (string.IsNullOrEmpty(title))
                return MCPContent.CreateSimpleContent("❌ 'title' required for new multimedia record");

            if (string.IsNullOrEmpty(filePath))
                return MCPContent.CreateSimpleContent("❌ 'file_path' required for new multimedia record");

            if (string.IsNullOrEmpty(mediaTypeStr))
                return MCPContent.CreateSimpleContent("❌ 'media_type' required for new multimedia record");

            if (string.IsNullOrEmpty(storeTypeStr))
                return MCPContent.CreateSimpleContent("❌ 'store_type' required for new multimedia record");

            if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                return MCPContent.CreateSimpleContent($"❌ Invalid media type: '{mediaTypeStr}'.");

            if (!RuntimeData.StoreTypeMap.TryGetValue(storeTypeStr, out var storeType))
                return MCPContent.CreateSimpleContent($"❌ Invalid store type: '{storeTypeStr}'.");

            var tree = baseContext.Tree;

            var mediaRec = new GDMMultimediaRecord(tree);
            mediaRec.ChangeDate.ChangeDateTime = DateTime.Now;
            tree.NewXRef(mediaRec);

            var fileRef = mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
            bool saved = baseContext.MediaSave(fileRef, filePath, storeType);
            if (!saved) {
                return MCPContent.CreateSimpleContent($"❌ Failed to save multimedia file: '{filePath}' with store type '{storeTypeStr}'.");
            }

            fileRef.MediaType = mediaType;
            fileRef.Title = title;
            tree.AddRecord(mediaRec);

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Multimedia record added: {mediaRec.XRef} - \"{title}\" ({mediaTypeStr}, {storeTypeStr})");
        }
    }
}
