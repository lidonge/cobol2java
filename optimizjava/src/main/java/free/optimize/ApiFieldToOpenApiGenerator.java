package free.optimize;

/**
 * @author lidong@date 2024-12-12@version 1.0
 */
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.responses.ApiResponses;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.core.util.Yaml;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class ApiFieldToOpenApiGenerator {

    public static void main(String[] args) {
        try {
            List<ApiField> apiFields = getApiFields(); // 自定义方法获取 ApiField 列表
            OpenAPI openAPI = generateOpenApiWithPaths("test",apiFields);
            saveAsYaml(openAPI, "openapi_with_paths.yaml");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static OpenAPI generateOpenApiWithPaths(String serviceName, List<ApiField> apiFields) {
        OpenAPI openAPI = new OpenAPI();

        // 设置基本信息
        openAPI.info(new Info()
                .title("API with Dynamic Paths")
                .version("1.0.0")
                .description("This API includes dynamically generated paths based on ApiField configurations.")
        );
        openAPI.addServersItem(new Server().url("https://api.servpp.com"));

        // 创建 Components
        Components components = new Components();
        components.addRequestBodies(serviceName+"RequestBody", generateRequestBody(apiFields));
        components.addResponses(serviceName+"Response", generateResponse(apiFields));
        openAPI.components(components);

        // 动态创建路径
        PathItem servicePath = new PathItem();

        // GET 方法
//        servicePath.get(new Operation()
//                .summary("Service "+serviceName)
//                .description(serviceName+" based on input parameters.")
//                .addParametersItem(generateParameter("id", "The unique identifier of the example.", true))
//                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().$ref("#/components/responses/ExampleResponse")))
//        );

        // POST 方法
//        servicePath.post(new Operation()
//                .summary("Create Example")
//                .description("Creates a new example.")
//                .requestBody(new RequestBody().$ref("#/components/requestBodies/ExampleRequestBody"))
//                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().$ref("#/components/responses/ExampleResponse")))
//        );

        // PUT 方法
        servicePath.put(new Operation()
                .summary("Update "+serviceName)
                .description("Updates an existing "+serviceName)
                .requestBody(new RequestBody().$ref("#/components/requestBodies/"+serviceName+"RequestBody"))
                .responses(new ApiResponses().addApiResponse("200", new ApiResponse().$ref("#/components/responses/"+serviceName+"Response")))
        );

        // DELETE 方法
//        servicePath.delete(new Operation()
//                .summary("Delete Example")
//                .description("Deletes an example by ID.")
//                .addParametersItem(generateParameter("id", "The unique identifier of the example to delete.", true))
//                .responses(new ApiResponses().addApiResponse("204", new ApiResponse().description("No Content")))
//        );

        // 添加路径到 OpenAPI 对象
        openAPI.path("/"+serviceName, servicePath);

        return openAPI;
    }

    private static RequestBody generateRequestBody(List<ApiField> apiFields) {
        int count = 0;
        ObjectSchema schema = new ObjectSchema();
        for (ApiField field : apiFields) {
            if (field.isIn() || !field.isOut()) {
                schema.addProperties(field.getQualifiedName(), new Schema<>()
                        .type(field.getType().asString().toLowerCase()) // 假设所有输入字段为字符串类型
                        .description(field.getInStmt()));
                count++;
            }
        }
        System.out.println("Generate " + count + " Request fields.");
        MediaType mediaType = new MediaType().schema(schema);
        return new RequestBody()
                .description("Request body for creating or updating an example.")
                .content(new Content().addMediaType("application/json", mediaType));
    }

    private static ApiResponse generateResponse(List<ApiField> apiFields) {
        ObjectSchema schema = new ObjectSchema();
        int count = 0;
        for (ApiField field : apiFields) {
            if (field.isOut()) {
                schema.addProperties(field.getQualifiedName(), new Schema<>()
                        .type(field.getType().asString().toLowerCase()) // 假设所有输出字段为字符串类型
                        .description(field.getOutStmt()));
                count++;
            }
        }
        System.out.println("Generate " + count + " Response fields.");
        MediaType mediaType = new MediaType().schema(schema);
        return new ApiResponse()
                .description("Successful response containing output fields.")
                .content(new Content().addMediaType("application/json", mediaType));
    }

    private static Parameter generateParameter(String name, String description, boolean required) {
        return new Parameter()
                .name(name)
                .in("query")
                .description(description)
                .required(required)
                .schema(new Schema<>().type("string")); // 假设所有参数为字符串类型
    }

    public static void saveAsYaml(OpenAPI openAPI, String filePath) throws IOException {
        String yamlText = Yaml.pretty().writeValueAsString(openAPI);
        File path= new File(filePath).getParentFile();
        if (!path.exists()) {
            path.mkdirs();
        }
        try (FileWriter writer = new FileWriter(new File(filePath))) {
            writer.write(yamlText);
        }
        System.out.println("OpenAPI YAML 文件已生成: " + filePath);
    }

    public static List<ApiField> getApiFields() {
        // 返回模拟的 ApiField 列表
        // 实际应用中可以从配置文件、数据库或其他来源加载数据
        return List.of(
                new ApiField("id", null, null) {{
                    setIn(true);
                    setInStmt("The unique identifier for the example.");
                }},
                new ApiField("name", null, null) {{
                    setOut(true);
                    setOutStmt("The name of the example.");
                }}
        );
    }
}