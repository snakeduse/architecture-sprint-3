# Проект "Smart Home"

## Описание текущей системы (модель AsIs)

Проект "Smart Home" представляет собой приложение для управления домом. В данный момент поддерживается управление 
отоплением и мониторинга температуры в умном доме. Пользователи могут удаленно включать/выключать отопление, 
устанавливать желаемую температуру и просматривать текущую температуру через веб-интерфейс.

Диаграмма контекста показана ниже:  
![alt text](diagrams/images/ContextAsIs.svg)

Приложение выделяет следующие доменные области:
- Управление устройствами в домах;
- Работа с телеметрией;
- Управление пользователями;
- Управление домами и модулями;
- Техническая поддержка клиентов, работа с заявками от клиентов.

## Модернизация системы под высокие нагрузки (модель ToBe) 

Ниже представлена диаграмма контейнеров новой версии проекта "Smart Home", ориентированного на расширение 
и развитие согласно планам увеличения базы клиентов:

![alt text](./diagrams/images/Container.svg)

На диаграмме показаны следующие компоненты:
- `User` - Конечный пользователь системы "Smart Home", который регистрируется в системе и может добавлять свои устройства и управлять ими;
- `Administrator` - Администраторы системы "Smart Home". Работу с административными задачами и технической поддержкой конечных пользователей;
- `UI WebApplications` - Основное веб-приложение с пользовательским интерфейсом для взаимодействия с системой конечными пользователями и администраторами; 
- `API Gateway` - Точка входа в систему, выполняет маршрутизацию запросов, балансировку нагрузки, аутентификацию и авторизацию;
- `Users Service` - Сервис управления пользователями. В качестве базы данных используется реляционная база данных PostgreSQL;
- `Devices Service` - Сервис управления устройствами. В качестве базы данных используется реляционная база данных PostgreSQL;
- `Houses Service` - Сервис управления домами. В качестве базы данных используется реляционная база данных PostgreSQL;
- `Telemetry Service` - Сервис управления телеметрией устройств. В качестве базы данных используется NoSQL база данных MongoDB для дальнейшего развития массовой передачи телеметрии от устройств;
- `TechService Service` - Сервис технической поддержки клиентов. Сервис работает с заявками технической поддержки;
- `Devices` - Устройства, которые взаимодействуют с системой "Smart Home". Получают настройки для работы и передают информацию о телеметрии.

## Базовая настройка

### Запуск minikube

[Инструкция по установке](https://minikube.sigs.k8s.io/docs/start/)

```bash
minikube start
```


### Добавление токена авторизации GitHub

[Получение токена](https://github.com/settings/tokens/new)

```bash
kubectl create secret docker-registry ghcr --docker-server=https://ghcr.io --docker-username=<github_username> --docker-password=<github_token> -n default
```


### Установка API GW kusk

[Install Kusk CLI](https://docs.kusk.io/getting-started/install-kusk-cli)

```bash
kusk cluster install
```


### Настройка terraform

[Установите Terraform](https://yandex.cloud/ru/docs/tutorials/infrastructure-management/terraform-quickstart#install-terraform)


Создайте файл ~/.terraformrc

```hcl
provider_installation {
  network_mirror {
    url = "https://terraform-mirror.yandexcloud.net/"
    include = ["registry.terraform.io/*/*"]
  }
  direct {
    exclude = ["registry.terraform.io/*/*"]
  }
}
```

### Применяем terraform конфигурацию 

```bash
cd terraform
terraform apply
```

### Настройка API GW

```bash
kusk deploy -i api.yaml
```

### Проверяем работоспособность

```bash
kubectl port-forward svc/kusk-gateway-envoy-fleet -n kusk-system 8080:80
curl localhost:8080/hello
```


### Delete minikube

```bash
minikube delete
```
