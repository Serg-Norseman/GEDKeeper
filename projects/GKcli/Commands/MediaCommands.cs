/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
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


internal class MediaListCommand : BaseCommand
{
    public MediaListCommand() : base("multimedia_list", LSID.Find, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        PromptHelper.SelectRecord(baseContext, GDMRecordType.rtMultimedia, "Select a multimedia", "Multimedia: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all multimedia records in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtMultimedia);
        return MCPHelper.PageableTable("multimedia", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Title | Type | File |\n|---|---|---|---|";
            } else {
                var rec = (GDMMultimediaRecord)recList[index];
                var fileRef = rec.FileReferences.Count > 0 ? rec.FileReferences[0] : null;
                if (fileRef == null) return string.Empty;

                string title = fileRef.Title;
                string mediaType = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                string file = fileRef.StringValue;
                return $"|{rec.XRef}|{title}|{mediaType}|{file}|";
            }
        });
    }
}


internal class MediaAddCommand : BaseCommand
{
    private static Dictionary<string, GDMMediaType> MediaTypeMap;
    private static Dictionary<string, MediaStoreType> StoreTypeMap;

    public MediaAddCommand() : base("multimedia_add", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    private static void RequireMaps()
    {
        if (MediaTypeMap == null) {
            MediaTypeMap = new Dictionary<string, GDMMediaType>();
            for (GDMMediaType mt = GDMMediaType.mtUnknown; mt <= GDMMediaType.mtLast; mt++) {
                MediaTypeMap.Add(LangMan.LS(GKData.MediaTypes[(int)mt]), mt);
            }
        }

        if (StoreTypeMap == null) {
            StoreTypeMap = new Dictionary<string, MediaStoreType>();
            for (var st = MediaStoreType.mstReference; st <= MediaStoreType.mstURL; st++) {
                StoreTypeMap.Add(LangMan.LS(GKData.GKStoreTypes[(int)st].Name), st);
            }
        }
    }

    public override MCPTool CreateTool()
    {
        RequireMaps();

        //return string.Join(", ", result);
        var mediaTypes = MediaTypeMap.Keys.ToList();
        var storeTypes = StoreTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a new multimedia record to the database with a file reference",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the multimedia item" },
                    ["file_path"] = new MCPToolProperty { Type = "string", Description = "Path to the multimedia file on disk or URL" },
                    //["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file. Available values: " + GetMediaTypeValues() },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file.", Enum = mediaTypes },
                    //["store_type"] = new MCPToolProperty { Type = "string", Description = "Storage type for the multimedia file. Available values: " + GetStoreTypeValues() }
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

        if (!MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
            return MCPContent.CreateSimpleContent($"Invalid media type: '{mediaTypeStr}'.");

        if (!StoreTypeMap.TryGetValue(storeTypeStr, out var storeType))
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


internal class MediaDeleteCommand : BaseCommand
{
    public MediaDeleteCommand() : base("multimedia_delete", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a multimedia record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'M1')" }
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

        baseContext.DeleteRecord(mediaRec);

        return MCPContent.CreateSimpleContent($"Multimedia record deleted: {xref}");
    }
}
