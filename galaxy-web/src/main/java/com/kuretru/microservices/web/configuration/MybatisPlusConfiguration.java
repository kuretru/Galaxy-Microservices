package com.kuretru.microservices.web.configuration;

import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.core.handlers.MetaObjectHandler;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import com.kuretru.microservices.web.mapper.MyBatisPlusSqlInjector;
import org.apache.ibatis.reflection.MetaObject;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.time.Instant;

/**
 * MyBatis-Plus配置
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@EnableTransactionManagement
public class MybatisPlusConfiguration {

    @Bean
    public MybatisPlusInterceptor mybatisPlusInterceptor() {
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        // 启用分页插件
        interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.MYSQL));
        return interceptor;
    }

    @Bean
    public MyBatisPlusSqlInjector myBatisPlusSqlInjector() {
        return new MyBatisPlusSqlInjector();
    }

    @Component
    static class MyMetaObjectHandler implements MetaObjectHandler {

        @Override
        public void insertFill(MetaObject metaObject) {
            this.strictInsertFill(metaObject, "createTime", Instant.class, Instant.now());
            this.strictInsertFill(metaObject, "createBy", String.class, "kuretru");
            this.strictInsertFill(metaObject, "updateTime", Instant.class, Instant.now());
            this.strictInsertFill(metaObject, "updateBy", String.class, "kuretru");
        }

        @Override
        public void updateFill(MetaObject metaObject) {
            this.strictUpdateFill(metaObject, "updateTime", Instant.class, Instant.now());
            this.strictUpdateFill(metaObject, "updateBy", String.class, "kuretru");
        }

    }

}
