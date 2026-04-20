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
using GKCore;
using GKCore.Locales;
using GKCore.Media;
using GKUI.Platform;

namespace GKcli.Commands;

internal class MediaMenuCommand : BaseCommand
{
    public MediaMenuCommand() : base("media", LSID.RPMultimedia, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Multimedia, true, "Select a multimedia operation");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
internal class MediaListCommand : BaseCommand
{
    public MediaListCommand() : base("multimedia_list", LSID.Find, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtMultimedia, "Select a multimedia", "Multimedia: {0}", "No records.");
    }
}


internal class MediaAddCommand : BaseCommand
{
    public MediaAddCommand() : base("multimedia_add", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var mediaTypes = RuntimeData.MediaTypeMap.Keys.ToList();
        var storeTypes = RuntimeData.StoreTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a new multimedia record to the database with a file reference",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the multimedia item" },
                    ["file_path"] = new MCPToolProperty { Type = "string", Description = "Path to the multimedia file on disk or URL" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file.", Enum = mediaTypes },
                    ["store_type"] = new MCPToolProperty { Type = "string", Description = "Storage type for the multimedia file.", Enum = storeTypes }
                },
                Required = new List<string> { "title", "file_path", "media_type", "store_type" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string title = MCPHelper.GetRequiredArgument(args, "title");
        string filePath = MCPHelper.GetRequiredArgument(args, "file_path");
        string mediaTypeStr = MCPHelper.GetRequiredArgument(args, "media_type");
        string storeTypeStr = MCPHelper.GetRequiredArgument(args, "store_type");

        if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
            return MCPContent.CreateSimpleContent($"Invalid media type: '{mediaTypeStr}'.");

        if (!RuntimeData.StoreTypeMap.TryGetValue(storeTypeStr, out var storeType))
            return MCPContent.CreateSimpleContent($"Invalid store type: '{storeTypeStr}'.");

        var tree = baseContext.Tree;

        var mediaRec = new GDMMultimediaRecord(tree);
        mediaRec.ChangeDate.ChangeDateTime = DateTime.Now;
        tree.NewXRef(mediaRec);

        var fileRef = mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
        bool saved = baseContext.MediaSave(fileRef, filePath, storeType);
        if (!saved) {
            return MCPContent.CreateSimpleContent($"Failed to save multimedia file: '{filePath}' with store type '{storeTypeStr}'.");
        }

        fileRef.MediaType = mediaType;
        fileRef.Title = title;

        tree.AddRecord(mediaRec);
        baseContext.SetModified();

        string xref = mediaRec.XRef;
        return MCPContent.CreateSimpleContent($"Multimedia record added: {xref} - \"{title}\" ({mediaTypeStr}, {storeTypeStr})");
    }
}


internal class MediaEditCommand : BaseCommand
{
    public MediaEditCommand() : base("multimedia_edit", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var mediaTypes = RuntimeData.MediaTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing multimedia record in the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "New title/name of the multimedia item" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "New media type", Enum = mediaTypes },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found: '{xref}'.");

        var fileRef = mediaRec.FileReferences[0];

        string title = MCPHelper.GetStringArgument(args, "title", null);
        string mediaTypeStr = MCPHelper.GetStringArgument(args, "media_type", null);

        if (title != null) {
            fileRef.Title = title;
        }

        if (mediaTypeStr != null) {
            if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                return MCPContent.CreateSimpleContent($"Invalid media type: '{mediaTypeStr}'.");

            fileRef.MediaType = mediaType;
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Multimedia record updated: {xref} - \"{title}\" ({mediaTypeStr})");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class MediaDeleteCommand : BaseCommand
{
    public MediaDeleteCommand() : base("multimedia_delete", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class MediaGetCommand : BaseCommand
{
    public MediaGetCommand() : base("multimedia_get", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

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
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found with XRef: {xref}");

        GDMFileReferenceWithTitle fileRef = mediaRec.FileReferences[0];
        if (fileRef == null)
            return MCPContent.CreateSimpleContent($"Media record {xref} contains no files");

        if (!GKUtils.UseEmbeddedViewer(fileRef.GetMultimediaFormat())) {
            return MCPContent.CreateSimpleContent($"Media record {xref} could not be transferred");
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

                                    return MCPHelper.CreateImageContent(fs, mimeType);
                                } finally {
                                    fs.Dispose();
                                }
                            }
                            break;
                        }

                    case MultimediaKind.mkAudio:
                    case MultimediaKind.mkVideo:
                        return MCPContent.CreateSimpleContent($"Media record {xref} could not be transferred (audio/video)");

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
                                        return MCPContent.CreateSimpleContent(text);
                                    } else {
                                        return MCPContent.CreateSimpleContent($"Media record {xref} could not be transferred (unsupported text format)");
                                    }
                                } finally {
                                    fs.Dispose();
                                }
                            }
                            break;
                        }
                }
            } catch (Exception ex) {
                Logger.WriteError("MediaGetCommand.ExecuteTool().Transfer", ex);
                return MCPContent.CreateSimpleContent($"Media record {xref} caused an error");
            }
        }

        return MCPContent.CreateSimpleContent($"Multimedia record received: {xref}");
    }
}
