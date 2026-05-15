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
using GKcli.Platform;
using GKCore;
using GKCore.Kinships;
using GKCore.Tools;

namespace GKcli.Features;


internal class PedigreeTraverseTool : BaseTool
{
    public PedigreeTraverseTool() : base("pedigree_traverse") { }

    public override MCPTool CreateTool()
    {
        var directions = RuntimeData.TraverseDirectionMap.Keys.ToList();
        var outputFormats = new List<string> { "md_table", "json_graph" };

        return new MCPTool {
            Name = Sign,
            Description = "Traverse pedigree in a specified direction from an individual",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["direction"] = new MCPToolProperty { Type = "string", Description = "Direction to traverse", Enum = directions },
                    ["depth"] = new MCPToolProperty { Type = "integer", Description = "Depth to traverse (default: 1)", Default = 1 },
                    ["output_format"] = new MCPToolProperty { Type = "string", Description = "Output format: 'md_table' or 'json_graph'", Enum = outputFormats, Default = "md_table" }
                },
                Required = new List<string> { "individual_xref", "direction" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        string directionStr = MCPHelper.GetRequiredStr(args, "direction").ToLowerInvariant();
        int depth = MCPHelper.GetOptionalInt(args, "depth", 1);
        string outputFormat = MCPHelper.GetOptionalStr(args, "output_format", "md_table");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"❌ Individual not found with XRef: {individualXRef}");

        if (!RuntimeData.TraverseDirectionMap.TryGetValue(directionStr, out TreeWalkMode direction))
            return MCPContent.CreateSimpleContent($"❌ Invalid direction: '{directionStr}'.");

        TraverseData traverseData = new TraverseData();
        traverseData.Depth = depth;

        if (outputFormat == "json_graph") {
            // JSON graph output mode
            TreeTools.WalkTree(baseContext.Tree, indiRec, direction, GraphWalkProc, traverseData);

            // Create JSON graph structure
            var graph = new {
                nodes = traverseData.Nodes.Values.ToList(),
                links = traverseData.Links
            };

            string jsonOutput = JsonSerializer.Serialize(graph, new JsonSerializerOptions { WriteIndented = false });
            return MCPContent.CreateSimpleContent(jsonOutput);
        } else {
            // Default MD table output mode
            traverseData.Result.Add($"Traversing {directionStr} of {individualXRef} for {depth} levels:");
            traverseData.Result.Add("| Generation | Individual XRef | Name | Birth Date | Death Date |");
            traverseData.Result.Add("|---|---|---|---|---|");

            TreeTools.WalkTree(baseContext.Tree, indiRec, direction, TableWalkProc, traverseData);

            return MCPContent.CreateSimpleContent(string.Join("\n", traverseData.Result));
        }
    }

    private bool TableWalkProc(GDMIndividualRecord iRec, GDMIndividualRecord prevRec, KinshipType kinshipType, int generation, TreeWalkMode mode, TraverseData extData)
    {
        bool resContinue = (iRec != null && !extData.Visited.Contains(iRec.XRef) && Math.Abs(generation) <= extData.Depth);
        if (resContinue) {
            string name = GKUtils.GetNameString(iRec, false);
            var lifeEvents = iRec.GetLifeEvents();
            var birthDate = GKUtils.GetDateDisplayString(lifeEvents.BirthEvent?.Date);
            var deathDate = GKUtils.GetDateDisplayString(lifeEvents.DeathEvent?.Date);

            extData.Result.Add($"| {generation} | {iRec.XRef} | {name} | {birthDate} | {deathDate} |");
        }
        return resContinue;
    }

    // ktNone, ktUndefined, ktSame, ktParent, ktSpouse, ktChild, ktFather, ktMother
    private string[] KinshipTypes = new string[] {
        "", "", "", "parent", "spouse", "child", "father", "mother"
    };

    private bool GraphWalkProc(GDMIndividualRecord iRec, GDMIndividualRecord prevRec, KinshipType kinshipType, int generation, TreeWalkMode mode, TraverseData extData)
    {
        bool resContinue = (iRec != null && Math.Abs(generation) <= extData.Depth);
        if (resContinue) {
            // Add node if not already added
            if (!extData.Nodes.ContainsKey(iRec.XRef)) {
                //var lifeEvents = iRec.GetLifeEvents();
                //var birthDate = GKUtils.GetDateDisplayString(lifeEvents.BirthEvent?.Date);
                //var deathDate = GKUtils.GetDateDisplayString(lifeEvents.DeathEvent?.Date);

                var node = new GraphNode {
                    id = iRec.XRef,
                    name = GKUtils.GetNameString(iRec, false),
                    //generation = generation,
                    //birthDate = birthDate,
                    //deathDate = deathDate
                };

                extData.Nodes.Add(iRec.XRef, node);
            }

            // Add relationships
            if (kinshipType != KinshipType.ktNone && prevRec != null) {
                string ksType = KinshipTypes[(int)kinshipType];
                extData.Links.Add(new GraphLink { source = prevRec.XRef, target = iRec.XRef, type = ksType });
            }
        }
        return resContinue;
    }

    private class TraverseData
    {
        public int Depth;

        public HashSet<string> Visited = new HashSet<string>();
        public List<string> Result = new List<string>();

        public Dictionary<string, GraphNode> Nodes = new Dictionary<string, GraphNode>();
        public List<GraphLink> Links = new List<GraphLink>();
    }

    private class GraphNode
    {
        public string id { get; set; }
        public string name { get; set; }
        //public int generation { get; set; }
        //public string birthDate { get; set; }
        //public string deathDate { get; set; }
    }

    private class GraphLink
    {
        public string source { get; set; }
        public string target { get; set; }
        public string type { get; set; }
    }
}
