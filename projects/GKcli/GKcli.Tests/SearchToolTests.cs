using System.Text.Json;
using GKcli.MCP;
using NUnit.Framework;

namespace GKcli.Tests;

[TestFixture]
public class SearchToolTests : MCPToolTests
{
    public SearchToolTests() : base()
    {
    }

    [Test]
    [TestCase("add individual", "individual_upsert")]
    [TestCase("add family", "family_upsert")]
    [TestCase("individual add edit event", "individual_upsert_event")]
    [TestCase("add edit individual", "individual_upsert")]
    [TestCase("create individual family person", "individual_upsert")]
    [TestCase("create individual family list person events", "family_list_events")] // 0
    [TestCase("create individual family list person events", "individual_list_events")] // 1
    [TestCase("create individual family list person events", "individual_upsert_event")] // 2
    [TestCase("create individual family list person events", "family_upsert_event")] // 3
    [TestCase("create individual family list person events", "individual_upsert")] // 4
    [TestCase("create individual family list person events", "family_upsert")] // 5
    public void Test_Search(string query, string toolName)
    {
        var instance = new SearchTool();
        Assert.IsNotNull(instance);

        string jsonString = "{ \"query\": \"{0}\" }".Replace("{0}", query);
        using (JsonDocument doc = JsonDocument.Parse(jsonString)) {
            JsonElement args = doc.RootElement;

            var result = instance.ExecuteTool(fContext, args);
            Assert.IsNotNull(result);
            Assert.IsTrue(result.Count > 0);
            Assert.IsTrue(ExistsTool(result, toolName));
        }
    }

    private static bool ExistsTool(List<MCPContent> result, string toolName)
    {
        string tn = $"\"{toolName}\"";
        return result.Where(r => r.Text.Contains(tn)).Any();
    }
}
