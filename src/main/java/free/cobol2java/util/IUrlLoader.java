package free.cobol2java.util;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * @author lidong@date 2024-09-10@version 1.0
 */
public interface IUrlLoader {
    default String getString( URI antlrUri) throws IOException {
        InputStream reader = getInputStreamFromURI(antlrUri);
        byte[] bytes = reader.readAllBytes();
        reader.close();
        String str = new String(bytes);
        return str;
    }

    private static InputStream getInputStreamFromURI(URI uri) throws IOException {
        switch (uri.getScheme()) {
            case "file":
                // 处理文件 URI
                return getInputStreamFromFile(uri);
            case "http":
            case "https":
                // 处理 HTTP/HTTPS URI
                return getInputStreamFromHttp(uri);
            case "jar":
                // 处理 JAR 文件中的资源 URI
                return getInputStreamFromJar(uri);
            default:
                throw new IllegalArgumentException("Unsupported URI scheme: " + uri.getScheme());
        }
    }

    private static InputStream getInputStreamFromFile(URI uri) throws IOException {
        File file = new File(uri);
        return Files.newInputStream(Paths.get(file.toURI()));
    }

    private static InputStream getInputStreamFromHttp(URI uri) throws IOException {
        URL url = uri.toURL();
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        return connection.getInputStream();
    }

    private static InputStream getInputStreamFromJar(URI uri) throws IOException {
        InputStream inputStream = uri.toURL().openStream();
        if (inputStream == null) {
            throw new IOException("Resource not found: " + uri);
        }
        return inputStream;
    }
}
