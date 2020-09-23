-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE coffeetypes AS ENUM ('americano', 'latte', 'irish_coffee', 'cappuccino', 'espresso', 'flat_white', 'glace', 'lungo', 'espresso_romano', 'iced_coffee', 'marochino', 'freddo', 'mocha');
CREATE TABLE coffees (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    labels TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    coffee_type coffeetypes NOT NULL,
    last_drank DATE NOT NULL
);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    email TEXT NOT NULL,
    github_name TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE comments (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    coffee_id UUID NOT NULL,
    user_id UUID NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
ALTER TABLE comments ADD CONSTRAINT comments_ref_coffee_id FOREIGN KEY (coffee_id) REFERENCES coffees (id) ON DELETE NO ACTION;
ALTER TABLE comments ADD CONSTRAINT comments_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
