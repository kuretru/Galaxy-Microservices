# Api-Common-Utils

Java后端API开发通用工具

## 功能

## 使用方式

1. 创建一个工程目录parent
2. 在此目录下新建一个pom.xml文件，作为父工程

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.kuretru.web</groupId>
    <artifactId>aries-navigation-parent</artifactId>
    <version>1.0.0</version>
    <name>Aries-Navigation-Parent</name>
    <description>白羊导航-后端API-父项目</description>
    <packaging>pom</packaging>

    <modules>
        <module>api-common-utils</module>
        <module>aries-navigation-api</module>
    </modules>

</project>

```

3.下载本项目至api-common-utils，或使用git submodule的方式引入，若使用此方式，可以先fork此项目至您自己的仓库，方便二次修改

```bash
git submodule add git@github.com:kuretru/Api-Common-Utils.git "api-common-utils"
```

4.新建aries-navigation-api您自己的项目，并在此项目中引入api-common-utils依赖

```xml
<dependency>
    <groupId>com.kuretru.common</groupId>
    <artifactId>api-common-utils</artifactId>
    <version>0.4.0</version>
</dependency>
```

5.在自己项目的启动类中扫描`api-common-utils`的组件

```java
@SpringBootApplication
@ComponentScan({"com.kuretru.web.navigation", "com.kuretru.api.common"})
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }

}
```

6.至此，您的项目已拥有`api-common-utils`中的所有功能，您的parent目录结构看起来会是如下所示：

```bash
.
├── api-common-utils
│   ├── pom.xml
│   ├── src
├── aries-navigation-api
│   ├── pom.xml
│   ├── src
└── pom.xml
```
