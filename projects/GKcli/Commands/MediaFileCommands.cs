/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class MediaListFilesCommand : BaseCommand
{
    public MediaListFilesCommand() : base("multimedia_list_files", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all files of a multimedia record by its XRef identifier",
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
            return MCPContent.CreateSimpleContent($"Multimedia record not found with XRef: {xref}");

        if (mediaRec.FileReferences.Count <= 0)
            return MCPContent.CreateSimpleContent($"Multimedia record '{xref}' has no file references.");

        var rows = new List<string> {
            $"Files for multimedia record '{xref}' ({mediaRec.FileReferences.Count}):",
            "| Index | Title | Media Type |",
            "|---|---|---|"
        };
        for (int i = 0; i < mediaRec.FileReferences.Count; i++) {
            var fileRef = mediaRec.FileReferences[i];
            string mediaTypeStr = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
            rows.Add($"|{i}|{fileRef.Title}|{mediaTypeStr}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class MediaAddFileCommand : BaseCommand
{
    public MediaAddFileCommand() : base("multimedia_add_file", null, CommandCategory.Multimedia) { }

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
            Description = "Add a file to an existing multimedia record",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'O1')" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the multimedia file" },
                    ["file_path"] = new MCPToolProperty { Type = "string", Description = "Path to the multimedia file on disk or URL" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file.", Enum = mediaTypes },
                    ["store_type"] = new MCPToolProperty { Type = "string", Description = "Storage type for the multimedia file.", Enum = storeTypes }
                },
                Required = new List<string> { "xref", "title", "file_path", "media_type", "store_type" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        string title = MCPHelper.GetRequiredStr(args, "title");
        string filePath = MCPHelper.GetRequiredStr(args, "file_path");
        string mediaTypeStr = MCPHelper.GetRequiredStr(args, "media_type");
        string storeTypeStr = MCPHelper.GetRequiredStr(args, "store_type");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found: '{xref}'.");

        if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
            return MCPContent.CreateSimpleContent($"Invalid media type: '{mediaTypeStr}'.");

        if (!RuntimeData.StoreTypeMap.TryGetValue(storeTypeStr, out var storeType))
            return MCPContent.CreateSimpleContent($"Invalid store type: '{storeTypeStr}'.");

        var fileRef = mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
        bool saved = baseContext.MediaSave(fileRef, filePath, storeType);
        if (!saved) {
            mediaRec.FileReferences.Remove(fileRef);
            return MCPContent.CreateSimpleContent($"Failed to save multimedia file: '{filePath}' with store type '{storeTypeStr}'.");
        }

        fileRef.MediaType = mediaType;
        fileRef.Title = title;

        baseContext.SetModified();

        int fileIndex = mediaRec.FileReferences.IndexOf(fileRef);
        return MCPContent.CreateSimpleContent($"File added to multimedia record '{xref}' at index {fileIndex}: \"{title}\" ({mediaTypeStr}, {storeTypeStr})");
    }
}


internal class MediaEditFileCommand : BaseCommand
{
    public MediaEditFileCommand() : base("multimedia_edit_file", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var mediaTypes = RuntimeData.MediaTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit a file of a multimedia record by record XRef and file index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'O1')" },
                    ["file_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the file in the multimedia record's file list" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "New title/name of the multimedia file" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "New media type", Enum = mediaTypes }
                },
                Required = new List<string> { "xref", "file_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        int fileIndex = MCPHelper.GetRequiredInt(args, "file_index");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found: '{xref}'.");

        if (mediaRec.FileReferences.Count <= 0)
            return MCPContent.CreateSimpleContent($"Multimedia record '{xref}' has no file references.");

        if (fileIndex < 0 || fileIndex >= mediaRec.FileReferences.Count)
            return MCPContent.CreateSimpleContent($"Invalid file index {fileIndex} for multimedia record '{xref}' (has {mediaRec.FileReferences.Count} files).");

        var fileRef = mediaRec.FileReferences[fileIndex];

        string title = MCPHelper.GetOptionalStr(args, "title", null);
        string mediaTypeStr = MCPHelper.GetOptionalStr(args, "media_type", null);

        if (title != null) {
            fileRef.Title = title;
        }

        if (mediaTypeStr != null) {
            if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                return MCPContent.CreateSimpleContent($"Invalid media type: '{mediaTypeStr}'.");

            fileRef.MediaType = mediaType;
        }

        baseContext.SetModified();

        string fileInfo = $"\"{fileRef.Title}\", media type '{fileRef.MediaType}'";
        return MCPContent.CreateSimpleContent($"File updated in multimedia record '{xref}' at index {fileIndex}: {fileInfo}");
    }
}


internal class MediaDeleteFileCommand : BaseCommand
{
    public MediaDeleteFileCommand() : base("multimedia_delete_file", null, CommandCategory.Multimedia) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a file from a multimedia record by record XRef and file index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'O1')" },
                    ["file_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the file in the multimedia record's file list" }
                },
                Required = new List<string> { "xref", "file_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        int fileIndex = MCPHelper.GetRequiredInt(args, "file_index");

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"Multimedia record not found: '{xref}'.");

        if (mediaRec.FileReferences.Count <= 0)
            return MCPContent.CreateSimpleContent($"Multimedia record '{xref}' has no file references.");

        if (fileIndex < 0 || fileIndex >= mediaRec.FileReferences.Count)
            return MCPContent.CreateSimpleContent($"Invalid file index {fileIndex} for multimedia record '{xref}' (has {mediaRec.FileReferences.Count} files).");

        var fileRef = mediaRec.FileReferences[fileIndex];
        string fileInfo = $"\"{fileRef.Title}\", media type '{fileRef.MediaType}'";

        mediaRec.FileReferences.RemoveAt(fileIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"File removed from multimedia record '{xref}' at index {fileIndex}: {fileInfo}");
    }
}
