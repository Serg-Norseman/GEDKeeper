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
using GKcli.Platform;
using GKCore;
using GKCore.Locales;

namespace GKcli.Features;

internal class MediaListFilesTool : BaseTool
{
    public MediaListFilesTool() : base("multimedia_list_files") { }

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


internal class MediaUpsertFileTool : BaseTool
{
    public MediaUpsertFileTool() : base("multimedia_upsert_file") { }

    public override MCPTool CreateTool()
    {
        var mediaTypes = RuntimeData.MediaTypeMap.Keys.ToList();
        var storeTypes = RuntimeData.StoreTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add new file to multimedia record or update existing. Provide 'file_index' to edit; omit 'file_index' to create. 'xref' and 'title' required for new files.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the multimedia record (e.g., 'O1')" },
                    ["file_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the file in the multimedia record's file list (omit for new)" },
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Title/name of the multimedia file" },
                    ["file_path"] = new MCPToolProperty { Type = "string", Description = "Path to the multimedia file on disk or URL (required for new)" },
                    ["media_type"] = new MCPToolProperty { Type = "string", Description = "Media type of the file.", Enum = mediaTypes },
                    ["store_type"] = new MCPToolProperty { Type = "string", Description = "Storage type for the multimedia file.", Enum = storeTypes }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        int? fileIndex = MCPHelper.GetOptionalNullableInt(args, "file_index", null);
        string title = MCPHelper.GetOptionalStr(args, "title", null);
        string filePath = MCPHelper.GetOptionalStr(args, "file_path", null);
        string mediaTypeStr = MCPHelper.GetOptionalStr(args, "media_type", null);
        string storeTypeStr = MCPHelper.GetOptionalStr(args, "store_type", null);

        var mediaRec = baseContext.Tree.FindXRef<GDMMultimediaRecord>(xref);
        if (mediaRec == null)
            return MCPContent.CreateSimpleContent($"❌ Multimedia record not found: '{xref}'.");

        bool isEdit = fileIndex.HasValue;
        if (isEdit) {
            int index = fileIndex.Value;

            if (mediaRec.FileReferences.Count <= 0)
                return MCPContent.CreateSimpleContent($"❌ Multimedia record '{xref}' has no file references.");

            if (index < 0 || index >= mediaRec.FileReferences.Count)
                return MCPContent.CreateSimpleContent($"❌ Invalid file index {index} for multimedia record '{xref}' (has {mediaRec.FileReferences.Count} files).");

            var fileRef = mediaRec.FileReferences[index];

            if (title != null) {
                fileRef.Title = title;
            }

            if (mediaTypeStr != null) {
                if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                    return MCPContent.CreateSimpleContent($"❌ Invalid media type: '{mediaTypeStr}'.");

                fileRef.MediaType = mediaType;
            }

            baseContext.SetModified();
            string fileInfo = $"\"{fileRef.Title}\", media type '{fileRef.MediaType}'";
            return MCPContent.CreateSimpleContent($"✅ File updated in multimedia record '{xref}' at index {index}: {fileInfo}");
        } else {
            if (string.IsNullOrEmpty(title))
                return MCPContent.CreateSimpleContent("❌ 'title' required for new file");

            if (string.IsNullOrEmpty(filePath))
                return MCPContent.CreateSimpleContent("❌ 'file_path' required for new file");

            if (string.IsNullOrEmpty(mediaTypeStr))
                return MCPContent.CreateSimpleContent("❌ 'media_type' required for new file");

            if (string.IsNullOrEmpty(storeTypeStr))
                return MCPContent.CreateSimpleContent("❌ 'store_type' required for new file");

            if (!RuntimeData.MediaTypeMap.TryGetValue(mediaTypeStr, out var mediaType))
                return MCPContent.CreateSimpleContent($"❌ Invalid media type: '{mediaTypeStr}'.");

            if (!RuntimeData.StoreTypeMap.TryGetValue(storeTypeStr, out var storeType))
                return MCPContent.CreateSimpleContent($"❌ Invalid store type: '{storeTypeStr}'.");

            var fileRef = mediaRec.FileReferences.Add(new GDMFileReferenceWithTitle());
            bool saved = baseContext.MediaSave(fileRef, filePath, storeType);
            if (!saved) {
                mediaRec.FileReferences.Remove(fileRef);
                return MCPContent.CreateSimpleContent($"❌ Failed to save multimedia file: '{filePath}' with store type '{storeTypeStr}'.");
            }

            fileRef.MediaType = mediaType;
            fileRef.Title = title;

            baseContext.SetModified();
            int newIndex = mediaRec.FileReferences.IndexOf(fileRef);
            return MCPContent.CreateSimpleContent($"✅ File added to multimedia record '{xref}' at index {newIndex}: \"{title}\" ({mediaTypeStr}, {storeTypeStr})");
        }
    }
}


internal class MediaDeleteFileTool : BaseTool
{
    public MediaDeleteFileTool() : base("multimedia_delete_file") { }

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
