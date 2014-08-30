
create table account (
    id bigserial primary key,
    nickname varchar(64),
    email varchar(64),
    password varchar(512) null,
    admin boolean not null default false,
    views integer not null default 0,
    created timestamp not null,
    modified timestamp not null,
    accessed_approx timestamp not null,
    constraint unique_account_nickname unique (nickname),
    constraint unique_account_email unique (email)
);

create unique index account_nickname_i on account (nickname);
create unique index account_email_i on account (email);


create table profile (
    id bigserial primary key,
    account bigint not null references account,
    timezone varchar(128),
    cookie varchar(256),
    episode_links varchar(8196),
    created timestamp not null,
    modified timestamp not null,
    constraint unique_profile_account unique (account)
    constraint unique_profile_cookie unique (cookie)
);

create unique index profile_cookie_i on profile (cookie);


create table show (
    id bigserial primary key,
    title varchar(256) not null,
    tv_rage_id integer null,
    subscription_count integer not null,
    created timestamp not null,
    modified timestamp not null,
    constraint unique_tv_rage_id unique (tv_rage_id)
);

create index show_subscription_count_i on show (subscription_count);
create index show_title_i on show(title);
create unique index show_tv_rage_id_i on show (tv_rage_id);


create table season (
    id bigserial primary key,
    number integer not null,
    show bigint not null references show,
    created timestamp not null,
    modified timestamp not null,
    constraint unique_show_season_number unique (show, number)
);


create table episode (
    id bigserial primary key,
    number integer not null,
    title varchar(512) not null,
    season bigint not null references season,
    air_date_time timestamp not null,
    view_count integer not null default 0,
    created timestamp not null,
    modified timestamp not null,
    constraint unique_episode_season_number unique (season, number)
);

create index episode_air_date_time_i on episode (air_date_time);
create index episode_season_i on episode (season);
create index episode_title_i on episode (title);


create table episode_status (
    id bigserial primary key,
    account bigint not null references account,
    episode bigint not null references episode,
    status varchar(32) not null,
    created timestamp not null,
    modified timestamp not null,
    constraint unique_episode_status_account_episode unique (account, episode)
);

create table subscription (
    id bigserial primary key,
    account bigint not null references account,
    show bigint not null references show,
    created timestamp not null,
    modified timestamp not null,
    constraint unique_subscription_account_show unique (account, show)
);
