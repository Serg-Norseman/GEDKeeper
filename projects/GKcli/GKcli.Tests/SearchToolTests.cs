using System.Text.Json;
using GKcli.Features;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using NUnit.Framework;

namespace GKcli.Tests;

[TestFixture]
public class SearchToolTests
{
    private readonly BaseContext fContext;

    public SearchToolTests()
    {
        LangMan.DefInit();
        MCPController.InitFeatures(pureMode: false, tdeMode: true, ragMode: true);

        fContext = new BaseContext(null);
    }

    [Test]
    [TestCase("add individual", "individual_upsert")]
    [TestCase("add family", "family_upsert")]
    [TestCase("individual add edit event", "individual_upsert_event")]
    [TestCase("add edit individual", "individual_upsert")]
    [TestCase("create individual family person", "individual_upsert")]
    //[TestCase("create individual family list person events", "individual_upsert")] - 12th place!
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
        return result.Where(r => r.Text.Contains(toolName)).Any();
    }
}
